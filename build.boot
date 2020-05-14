(set-env!
 :resource-paths #{"src" "resources"}
 :source-paths #{"test"}
 :dependencies '[[org.clojure/clojure "1.10.0"]

                 [adzerk/boot-test "1.2.0" :scope "test"]
                 [com.datomic/datomic-pro "0.9.6045"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]])

(require '[adzerk.boot-test :refer [test]])
(task-options! test {:include #"-test"})
