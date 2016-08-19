(ns dictionary.core-test
  (:require [clojure.walk :refer [postwalk]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [dictionary.core :refer :all]))

(def op-gen
  (gen/vector
    (gen/tuple (gen/elements [:add :retract])
               (gen/not-empty (gen/vector (gen/not-empty gen/string-alphanumeric)))
               gen/pos-int)))

(defn simulate [ops]
  (reduce
    (fn [res [op tokens]]
      (case op
        :add (conj res tokens)
        :retract (disj res tokens)))
    #{}
    ops))

(defn run [ops]
  (reduce
    (fn [dict [op tokens id]]
      (case op
        :add (add dict tokens id)
        :retract (retract dict tokens)))
    {}
    ops))

(defn paths
  ([m] (paths #{} m []))
  ([res dict path]
   (let [m (get-in dict path)]
     (reduce (fn [res k]
               (if (= :id k)
                 (conj res path)
                 (paths res dict (conj path k))))
             res
             (keys m)))))

(defn equivalent? [sim dict]
  (and (every? #(:id (get-in (:paths dict) %)) sim)
       (every? (partial contains? sim) (paths (:paths dict)))))

(defspec retains-all-paths
  (prop/for-all [ops op-gen]
    (equivalent? (simulate ops)
                 (run ops))))

(defspec perfect-match-with-threshold
  (prop/for-all [ops op-gen]
    (let [dict (run ops)]
      (every?
        (fn [[_ path id]]
          (= (all-matches dict path levenshtein 0.5)
             (all-matches dict path)))
        ops))))