(ns dictionary.core)

(defn levenshtein
  "Computes the Leventshtein distance between to strings. The strings must not be empty."
  [str1 str2]
  (let [^chars arr1  (into-array Character/TYPE str1)
        ^chars arr2  (into-array Character/TYPE str2)
        n            (alength arr1)
        m            (alength arr2)
        col-dim      (unchecked-inc (max m n))
        col          (int-array col-dim)
        prev-col-dim (unchecked-inc m)
        prev-col     (int-array col-dim (range prev-col-dim))]
    (dotimes [i n]
      (aset col 0 (unchecked-inc i))
      (dotimes [j m]
        (aset col
              (unchecked-inc j)
              (min
                (min (unchecked-inc (aget col j))
                     (unchecked-inc (aget prev-col (unchecked-inc j))))
                (unchecked-add-int (aget prev-col j)
                                   (if (= (aget arr1 i) (aget arr2 j)) 0 1)))))
      (dotimes [i prev-col-dim]
        (aset prev-col i (aget col i))))
    (aget col m)))

(defn add-max [state tokens]
  (update state (count tokens) (fnil inc 0)))

(defn retract-max [state tokens]
  (let [n    (count tokens)
        next (dec (get state n))]
    (if (pos? next)
      (assoc state n next)
      (dissoc state n))))

(defn add [dictionary tokens id]
  (-> (update dictionary
              :paths
              update-in tokens assoc :id id)
      (update :state (fnil add-max (sorted-map)) tokens)))

(defn retract [dictionary tokens]
  (if (:id (get-in (:paths dictionary) tokens))
    (-> (update dictionary :paths update-in tokens dissoc :id)
        (update :state (fnil retract-max (sorted-map)) tokens))
    dictionary))

(defn subseqs [tokens max]
  (when max
    (mapcat (fn [offset]
              (map
                #(take % (drop offset tokens))
                (range 1 (inc max))))
            (range 0 (count tokens)))))

(defn resolve-indexes [max result]
  (reduce
    (fn [res [idx id]]
      (let [start (int (/ idx max))
            end   (+ start (mod idx max))]
        (conj res {:start start
                   :end   end
                   :id    id})))
    []
    result))

(defn fuzzy-walk [paths distance-fn threshold pattern]
  (loop [paths          paths
         [step & steps] pattern]
    (if step
      (recur
        (or
          (get paths step)
          (->> (keys paths)
               (filter string?)
               (keep (fn [next-step]
                       (let [distance (distance-fn next-step step)]
                         (when (< distance (* threshold (count step)))
                           [distance next-step]))))
               (sort-by first)
               (first)
               (second)
               (get paths)))
        steps)
      paths)))

(defn all-matches
  [{:keys [paths state] :as dictionary} tokens & [distance-fn threshold]]
  (let [max       (first (last state))
        lookup-fn (if distance-fn
                    (partial fuzzy-walk paths distance-fn threshold)
                    (partial get-in paths))]
    (resolve-indexes
      max
      (reduce
        (fn [res [idx pattern]]
          (if-let [id (:id (lookup-fn pattern))]
            (assoc res idx id)
            res))
        {}
        (map-indexed vector (subseqs tokens max))))))