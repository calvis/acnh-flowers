(ns acnh.flowers
  (:require [clojure.math.combinatorics :as combo]
            [clojure.java.io :as io]
            [datomic.api :as d]))

(defn punnett-square
  "returns a list of all children implied by a punnett square of the two parents
  https://aiterusawato.github.io/guides/acnh/flowers.html#bookbreeding-algorithm"
  [parentA parentB]
  (let [genesA (apply combo/cartesian-product parentA)
        genesB (apply combo/cartesian-product parentB)]
    (into []
          (map (fn [[gA gB]]
                 (into [] (map (fn [b1 b2]
                                 (into [] (sort [b1 b2])))
                               gA gB))))
          (combo/cartesian-product genesA genesB))))

(defn breeding-outcomes
  "computes all possible children the two parents could have the chances
  of having that child, returned in the form
  #{[chance1 child1] [chance2 child2] ...}"
  [parentA parentB]
  (let [children (punnett-square parentA parentB)
        total-children (double (count children))]
    (into #{}
          (map (fn [[child count-child]]
                 [(/ count-child total-children) child]))
          (frequencies children))))

(def ternary->binary
  {0 [0 0]
   1 [0 1]
   2 [1 1]})

(def binary->ternary
  (clojure.set/map-invert ternary->binary))

(def genotype-breed-colors
  (clojure.edn/read-string (slurp (io/resource "colors.edn"))))

(def seeds
  (clojure.edn/read-string (slurp (io/resource "seeds.edn"))))

(def schema
  [[{:db/ident :flower/R
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/Y
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/W
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/B
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/color
     :db/valueType :db.type/keyword
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/breed
     :db/valueType :db.type/keyword
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/genotype
     :db/valueType :db.type/tuple
     :db/tupleAttrs [:flower/R :flower/Y :flower/W :flower/B]
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/id
     :db/valueType :db.type/tuple
     :db/tupleAttrs [:flower/breed :flower/R :flower/Y :flower/W :flower/B]
     :db/cardinality :db.cardinality/one
     :db/unique :db.unique/identity}
    {:db/ident :flower/seed?
     :db/valueType :db.type/boolean
     :db/cardinality :db.cardinality/one}
    {:db/ident :flower/score
     :db/valueType :db.type/long
     :db/cardinality :db.cardinality/one}
    ]

   [{:db/ident :path/parentA
     :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/one}
    {:db/ident :path/parentB
     :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/one}
    {:db/ident :path/child
     :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/one}
    {:db/ident :path/chance
     :db/valueType :db.type/double
     :db/cardinality :db.cardinality/one}
    {:db/ident :path/id
     :db/valueType :db.type/tuple
     :db/tupleAttrs [:path/parentA :path/parentB :path/chance :path/child]
     :db/cardinality :db.cardinality/one
     :db/unique :db.unique/identity}
    ]
   ])

(def path-pattern
  (let [flower-pattern [:db/id
                        :flower/id
                        :flower/color
                        :flower/genotype
                        :flower/seed?]]
    [:db/id
     {:path/parentA flower-pattern}
     {:path/parentB flower-pattern}
     {:path/child flower-pattern}
     :path/chance]))

(defn build-flowers-txn
  "builds a transaction that populates the given flower breed based on
  the genotype-breed-colors variable"
  [breed]
  (into []
        (mapcat (fn [[[R Y W B] types]]
                  (keep (fn [[breed' color]]
                          (when (= breed breed')
                            (merge
                             {:flower/id [breed' R Y W B] ;; for upsert
                              :flower/breed breed'
                              :flower/color color
                              :flower/R R
                              :flower/Y Y
                              :flower/W W}
                             (when B
                               {:flower/B B}))))
                        types)))
        genotype-breed-colors))

(defn build-seeds-txn
  "builds a transaction that notates all the seeds for the given breed"
  [breed]
  (into []
        (keep (fn [id]
                (when (= (first id) breed)
                  [:db/add [:flower/id id] :flower/seed? true])))
        seeds))

(defn build-parent-paths
  "builds all breeding paths (children) that can result from the two
  parent flowers"
  [db flowerA flowerB]
  (let [parentA (keep ternary->binary (:flower/genotype flowerA))
        parentB (keep ternary->binary (:flower/genotype flowerB))
        parentA-id (d/entid db [:flower/id (:flower/id flowerA)])
        parentB-id (d/entid db [:flower/id (:flower/id flowerB)])]
    (keep (fn [[chance child]]
            (let [[R Y W B] (map binary->ternary child)
                  child-id (d/entid db [:flower/id [(:flower/breed flowerA) R Y W B]])]
              (when (and child-id
                         (not= child-id parentA-id)
                         (not= child-id parentB-id)
                         (<= 0.05 chance))
                {:path/id [parentA-id parentB-id chance child-id]
                 :path/chance chance
                 :path/parentA parentA-id
                 :path/parentB parentB-id
                 :path/child child-id})))
          (breeding-outcomes parentA parentB))))

(defn build-paths!
  "given a connection that has flowers in it, populates all valid
  breeding paths between the flowers"
  [conn]
  (let [db (d/db conn)
        flowers (d/q '[:find [(pull ?flower [:flower/id
                                             :flower/breed
                                             :flower/genotype]) ...]
                       :in $
                       :where
                       [?flower :flower/genotype]]
                     db)
        parents (combo/cartesian-product flowers flowers)
        txn (into []
                  (mapcat (fn [[flowerA flowerB]]
                            (when (<= (hash flowerA) (hash flowerB)) ;; avoid having both AB and BA
                              (build-parent-paths db flowerA flowerB))))
                  parents)]
    @(d/transact conn txn)
    (count txn)))

(defn score-flowers!
  "ranks flowers according to their distance from seed"
  [conn]
  (loop [current (d/q '[:find [?flower ...]
                        :where
                        [?flower :flower/seed? true]]
                      (d/db conn))
         seed-distance 0]
    (when-not (empty? current)
      (let [txn (into []
                      (keep (fn [flower]
                              [:db/add flower :flower/score seed-distance]))
                      current)]
        @(d/transact conn txn)
        (let [next (d/q '[:find [?flower ...]
                          :in $ [?current ...]
                          :where
                          (or-join [?path ?current]
                            (and [?path :path/parentA ?current]
                                 [?path :path/parentB ?parentB]
                                 [?parentB :flower/score])
                            (and [?path :path/parentB ?current]
                                 [?path :path/parentA ?parentA]
                                 [?parentA :flower/score]))
                          [?path :path/child ?flower]
                          (not [?flower :flower/score])]
                        (d/db conn) current)]
          (recur next (inc seed-distance)))))))

(defn fresh-conn
  "builds a fresh database with all flowers, paths, scores,
  etc. populated"
  [breed]
  (let [uri (str "datomic:mem://" (gensym "flowers"))]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (doseq [txn (concat schema [(build-flowers-txn breed) (build-seeds-txn breed)])]
        @(d/transact conn txn))
      (build-paths! conn)
      (score-flowers! conn)
      conn)))

(defn best-parents
  "decides one set of best parents to choose, depending on the flower
  score"
  [db flower]
  (first (d/q '[:find ?path ?parentA ?parentB
                :in $ ?flower
                :where
                [?flower :flower/score ?score]
                [(dec ?score) ?parent-score]
                [?path :path/child ?flower]
                [?path :path/parentA ?parentA]
                [?path :path/parentB ?parentB]
                [?parentA :flower/score ?scoreA]
                [(< ?scoreA ?score)]
                [?parentB :flower/score ?scoreB]
                [(< ?scoreB ?score)]]
              db flower)))

(defn build-tree
  "given a set of desired outcomes, finds ONE tree (in practice, a list
  of breeding steps to accomplish) that achieves ONE of the outcomes.
  greedy; does not guarantee it is the best tree, just a tree that it
  can find rather quickly."
  [db outcomes]
  (let [outcome (->> (d/pull-many db [:db/id :flower/score] (seq outcomes))
                     (apply min-key :flower/score)
                     (:db/id))]
    (loop [needs [outcome]
           have #{}
           tree []]
      (if (empty? needs)
        tree
        (let [need (d/pull db [:db/id :flower/score :flower/seed?] (first needs))]
          (cond
            (:flower/seed? need)
            (recur (rest needs) (conj have (:db/id need)) tree)
            (contains? have (:db/id need))
            (recur (rest needs) have tree)
            :else
            (let [[path parentA parentB] (best-parents db (:db/id need))]
              (recur (concat [parentA parentB] (rest needs))
                     (conj have (:db/id need))
                     (conj tree path)))))))))
