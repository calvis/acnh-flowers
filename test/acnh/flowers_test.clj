(ns acnh.flowers-test
  (:require [clojure.test :refer :all]
            [acnh.flowers :refer :all]
            [datomic.api :as d]))

(defn pretty-path
  "test helper that gives readable output, such as:

  [:color/orange [1 1 0 nil] :color/orange [1 1 0 nil] 0.0625 :color/black [2 2 0 nil]]

  which translates to:

  orange 110
               6.25% chance => black 220
  orange 110
  "
  [path]
  (when path
    [(get-in path [:path/parentA :flower/color])
     (if (get-in path [:path/parentA :flower/seed?])
       :seed
       (get-in path [:path/parentA :flower/genotype]))
     (get-in path [:path/parentB :flower/color])
     (if (get-in path [:path/parentB :flower/seed?])
       :seed
       (get-in path [:path/parentB :flower/genotype]))
     (:path/chance path)
     (get-in path [:path/child :flower/color])
     (get-in path [:path/child :flower/genotype])]))

(defn with-color
  "test helper that computes the set of all flowers with the given
  color, for situations where the user only cares about the color and
  not the exact genotype of the flower"
  [db color]
  (into #{} (d/q '[:find [?flower ...] :in $ ?color :where [?flower :flower/color ?color]]
                 db color)))

(deftest test-punnett-square
  (let [parentA [[1 1]]
        parentB [[0 1]]]
    (is (= [[[0 1]]
            [[1 1]]
            [[0 1]]
            [[1 1]]]
           (punnett-square parentA parentB))))
  (let [parentA [[1 1] [0 0]]
        parentB [[0 1] [1 1]]]
    (is (= {[[0 1] [0 1]] 8
            [[1 1] [0 1]] 8}
           (frequencies (punnett-square parentA parentB))))))

(deftest test-breeding-outcomes
  ;; https://aiterusawato.github.io/satogu/acnh/flowers/advanced.html#breeding-example
  (testing "pansies example"
    (let [parentA [[1 1] [1 1] [0 1]]
          parentB [[0 1] [0 0] [1 1]]]
      (is (= #{[0.25 [[0 1] [0 1] [0 1]]]
               [0.25 [[0 1] [0 1] [1 1]]]
               [0.25 [[1 1] [0 1] [0 1]]]
               [0.25 [[1 1] [0 1] [1 1]]]}
             (breeding-outcomes parentA parentB)))))
  (testing "blue roses"
    (let [hybrid-red-rose [[0 1] [1 1] [1 1] [0 0]]
          blue-rose [[1 1] [1 1] [1 1] [0 0]]
          outcomes (breeding-outcomes hybrid-red-rose hybrid-red-rose)]
      (is (contains? outcomes [0.25 blue-rose])))))

(deftest test-fresh-conn
  (testing "mums"
    (let [conn (fresh-conn :breed/mums)]
      (is (= #{:color/yellow :color/pink :color/red :color/green :color/white :color/purple}
             (into #{} (d/q '[:find [?color ...]
                              :where
                              [?flower :flower/color ?color]]
                            (d/db conn)))))
      (is (= #{:color/yellow :color/red :color/white}
             (into #{} (d/q '[:find [?color ...]
                              :where
                              [?flower :flower/color ?color]
                              [?flower :flower/seed? true]]
                            (d/db conn)))))))
  (testing "cosmos"
    (let [conn (fresh-conn :breed/cosmos)]
      (is (= [{:flower/id [:breed/cosmos 2 2 0 nil]
	             :flower/color :color/black
	             :flower/score 2}
	            {:flower/id [:breed/cosmos 2 2 1 nil]
	             :flower/color :color/black
	             :flower/score 3}]
             (d/q '[:find [(pull ?flower [:flower/id :flower/color :flower/score]) ...]
                    :in $ ?color
                    :where
                    [?flower :flower/color ?color]]
                  (d/db conn) :color/black))))))

(deftest test-build-tree
  ;; https://imgur.com/a/Dy4wHXc

  (testing "cosmos"
    ;; red yellow => orange
    ;; red white => pink
    ;; orange orange => black
    (let [conn (fresh-conn :breed/cosmos)
          db (d/db conn)]
      (testing "pink"
        (let [tree (build-tree db (with-color db :color/pink))]
          (is (= [[:color/red :seed :color/white :seed 0.5 :color/pink [1 0 0 nil]]]
                 (map pretty-path (d/pull-many db path-pattern tree))))))
      (testing "black"
        (is (= [:color/orange [1 1 0 nil] :color/orange [1 1 0 nil] 0.0625 :color/black [2 2 0 nil]]
               (->> (best-parents db [:flower/id [:breed/cosmos 2 2 0 nil]])
                    (first)
                    (d/pull db path-pattern)
                    (pretty-path))))
        (let [tree (build-tree db (with-color db :color/black))]
          (is (= [[:color/orange [1 1 0 nil] :color/orange [1 1 0 nil] 0.0625 :color/black [2 2 0 nil]]
	                [:color/red :seed :color/yellow :seed 0.5 :color/orange [1 1 0 nil]]]
                 (map pretty-path (d/pull-many db path-pattern tree))))))))
  (testing "tulips"
    ;; red yellow => orange
    ;; red red => black
    ;; red white => pink
    ;; orange orange => purple
    (let [conn (fresh-conn :breed/tulip)
          db (d/db conn)]
      (testing "purple"
        (let [tree (build-tree db (with-color db :color/purple))]
          (is (= [[:color/orange [1 1 0 nil] :color/orange [1 1 0 nil] 0.0625 :color/purple [2 2 0 nil]]
	                [:color/yellow :seed :color/red :seed 0.5 :color/orange [1 1 0 nil]]]
                 (map pretty-path (d/pull-many db path-pattern tree))))))))
  (testing "mums"
    ;; red white => pink
    ;; white white => purp
    ;; red yellow => yellow*
    ;; yellow* yellow* => green (6%)
    ;; yellow* yellow* => purp*
    ;; purp* purp* => green (25%)
    (let [conn (fresh-conn :breed/mums)
          db (d/db conn)]
      (testing "purple"
        (is (= [[:color/white :seed :color/white :seed 0.25 :color/purple [0 0 2 nil]]]
               (map pretty-path (d/pull-many db path-pattern (build-tree db (with-color db :color/purple)))))))
      (testing "green"
        (is (= [[:color/yellow [1 1 0 nil] :color/yellow [1 1 0 nil] 0.0625 :color/green [2 2 0 nil]]
	              [:color/yellow :seed :color/red :seed 1.0 :color/yellow [1 1 0 nil]]]
               (map pretty-path (d/pull-many db path-pattern (build-tree db (with-color db :color/green)))))
            "it only finds the 6% strategy"))))
  (testing "roses"
    ;; red yellow => orange
    ;; red red => black
    ;; red red => pink
    ;; red white => pink
    ;; white white => purple*
    ;; black yellow => orange*
    ;; orange* purple* => red*
    ;; red* red* => blue
    (let [conn (fresh-conn :breed/rose)
          db (d/db conn)]
      (testing "blue"
        (is (= [[:color/red [1 1 2 0] :color/red [1 1 2 0] 0.0625 :color/blue [2 2 2 0]]
	              [:color/white [0 1 1 0] :color/red [1 0 1 0] 0.0625 :color/red [1 1 2 0]]
	              [:color/white :seed :color/yellow :seed 0.5 :color/white [0 1 1 0]]
	              [:color/white :seed :color/red :seed 0.25 :color/red [1 0 1 0]]]
               (map pretty-path (d/pull-many db path-pattern (build-tree db (with-color db :color/blue)))))
            "totally new strategy, short, but really bad %s")))))
