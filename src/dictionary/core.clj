(ns dictionary.core)

(defn add-max [state tokens]
  (update state (count tokens) (fnil inc 0)))

(defn retract-max [state tokens]
  (let [n (count tokens)
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
  (mapcat (fn [offset]
            (map
              #(take % (drop offset tokens))
              (range 1 (inc max))))
          (range 0 (count tokens))))

(defn resolve-indexes [max result]
  (reduce
    (fn [res [idx id]]
      (let [start (int (/ idx max))
            end (+ start (mod idx max))]
        (conj res {:start start
                   :end   end
                   :id    id})))
    []
    result))

(defn all-matches [dictionary tokens]
  (let [max (first (last (:state dictionary)))]
    (resolve-indexes max
      (reduce
        (fn [res [idx pattern]]
          (if-let [id (:id (get-in (:paths dictionary) pattern))]
            (assoc res idx id)
            res))
        {}
        (map-indexed vector (subseqs tokens max))))))