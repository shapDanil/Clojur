(ns untitled.core)

; Lab1.1

(defn add-head [set symbols]
  (if (empty? symbols)
    ()
    (if (not= (first set) (first symbols))
      (cons (cons (first symbols) set) (add-head set (rest symbols)))
      (add-head set (rest symbols)))))

(defn add-head [set symbols]
  (if (empty? symbols)
    ()
    ((let [foo ((add-head set (rest symbols))) per ((first symbols))]
       (if (not= (first set) per)
         (cons (cons per set) foo)
         ( foo ))))))

(defn gen-step-set [sets symbols]
  (if (empty? sets)
    ()
    (concat (add-head (first sets) symbols) (gen-step-set (rest sets) symbols))))
(defn step [sets symbols n]
  (if (= n 0)
    sets
    (step (gen-step-set sets symbols) symbols (dec n))))

(defn solve [symbols n]
  (if (>= n 0)
    (step (list(list)) symbols n)
    ()))

(println (solve (list "a" "b" "c") 3))
;(println my-even? [1 2 3 4 5 6 7 8 9])
;(println (empty? []) )
;(println (rest (list(list))))
;(println (first (list(list))) )
;(println (list(list)))
;(println (cons (first (list(list))) "combination"))