(ns Lab2.Lab2-2)
(defn area-calculation [func x y]
  (* (* (+ (func x) (func y)) 0.5) (- y x)))

(defn generate-lazy-seq [f step]
  (let [integr (fn [x1] (area-calculation f x1 (+ x1 step)))
        steps (iterate (fn [x] (+ x step)) 0)]
    (reductions + 0 (map integr steps))))

(defn integrate [func h]
  (let [integrate-seq (generate-lazy-seq func h)]
    (fn [x]
      (nth integrate-seq (quot x h)))))

(defn foo [x] (+ x 3))

(defn -main []
  (let [integral (integrate foo 0.5) ]
    (time (generate-lazy-seq foo 0.5))
    (time (integral 100))
    (time (integral 100))
    (time (integral 200))
    (time (integral 101))
    (time (integral 300))
    (time (integral 305))
    (time (integral 105))
    (time (integral 59))))