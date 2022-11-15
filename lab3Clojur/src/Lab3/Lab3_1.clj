(ns Lab3.Lab3-1)

(defn partition1 [n coll]
  (loop [n n coll coll acc (list)]
    (if (> (count coll) 0)
      (recur n (drop n coll) (concat acc (list (take n coll))))
      acc)))

(defn parallel-filter [func col thread_count]
  (let [n (int (Math/ceil (/ (count col) thread_count))) blocks (partition1 n col)]
    (mapcat deref (doall (map #(future (doall (filter func %))) blocks)))))

(defn foo [n]
  (do (Thread/sleep 100) 1)
  (even? n))

(defn -main []
  (time (doall (filter foo (range 50))))
  (time (doall (parallel-filter foo (range 50) 2)))
  (shutdown-agents))
