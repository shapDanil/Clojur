(ns Lab3.Lab3-2)

(defn partition1 [n coll]
  (when (not-empty coll)
    (lazy-seq (cons (take n coll) (partition n (drop n coll))))))

(defn lazy-parallel-filter [func coll count-elements-each-thread thread-count]
  (mapcat deref (mapcat #(doall (map (fn [x] (future (doall (filter func x)))) %))
                        (map #(partition1 count-elements-each-thread %) (partition1 (* thread-count count-elements-each-thread) coll)))))

(defn foo [n]
  (do (Thread/sleep 100) 1)
  (even? n))

(defn -main []
  (time (doall (take 20 (filter foo (range)))))
  (time (doall (take 20 (lazy-parallel-filter foo (range) 5 4))))
  (println (take 20 (lazy-parallel-filter foo (range) 5 4)))
  (shutdown-agents))