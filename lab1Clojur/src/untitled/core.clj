(ns untitled.core)

; Lab1.1
(defn add-head [set symbols]
  (if (empty? symbols)
    ()
    (if (not= (first set) (first symbols))
      (cons (cons (first symbols) set) (add-head set (rest symbols)))
      (add-head set (rest symbols))
      )
    )
  )
(defn gen-step-set [sets symbols]
  (if (empty? sets)
    ()
    (concat (add-head (first sets) symbols) (gen-step-set (rest sets) symbols))
    ))
(defn step [sets symbols n]
  (if (= n 0)
    sets
    (step (gen-step-set sets symbols) symbols (dec n))
    ))

(defn solve [symbols n]
  (if (>= n 0)
    (step (list(list)) symbols n)
    ()
    ))

(println (solve (list "a" "b" "c") 3))
;(println (empty? []) )
;(println (rest (list(list))))
;(println (first (list(list))) )
;(println (list(list)))
;(println (cons (first (list(list))) "combination"))