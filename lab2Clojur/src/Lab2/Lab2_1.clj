(ns Lab2.Lab2_1)


(defn area-calculation [func x y]
  (* (* (+ (func x) (func y)) 0.5) (- y x)))

(defn integral-mem [func h]
  (let [integrate (fn [integrate func h x]
                    (if (> x 0)
                      (+ (area-calculation func (- x h) x) (integrate integrate func h (- x h)))
                      0))
        mem-integrate (memoize integrate)]
    (partial mem-integrate mem-integrate func h)))
(defn integral[func h]
  (let [integrate (fn [integrate func h x]
                    (if (> x 0)
                      (+ (area-calculation func (- x h) x) (integrate integrate func h (- x h)))
                      0))
        mem-integrate integrate]
    (partial mem-integrate mem-integrate func h)))


(defn foo [x] (+ x 3))

(defn -main []
  (let [integral (integral foo 0.5) integral-mem (integral-mem foo 0.5)]
    (time (integral 100))
    (time (integral 100))
    (time (integral 100))
    (time (integral 100))
    (time (integral 100))
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 100))
    (time (integral-mem 105))
    (time (integral-mem 105))))

((integral foo 0.5) 3)
((integral foo 0.5) 1)
((integral foo 0.5) 10)