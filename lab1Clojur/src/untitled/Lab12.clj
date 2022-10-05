(ns untitled.Lab12)
(defn add-head [set symbols]
  (loop [set-loop set symbols-loop symbols sets-loop (list)]
    (if (empty? symbols-loop)
      sets-loop
      (if (not= (first set) (first symbols-loop))
        (recur set-loop (rest symbols-loop) (cons (cons (first symbols-loop) set-loop) sets-loop) )
        (recur set-loop (rest symbols-loop) sets-loop)
        )
      )
    )
  )
(defn gen-step-set [sets symbols]
  (loop [sets-loop sets  new-sets-loop (list)]
    (if (empty? sets-loop)
      new-sets-loop
      (recur (rest sets-loop) (concat new-sets-loop (add-head (first sets-loop) symbols))))))

(defn step [sets symbols n]
  (if (not= n 0)
    (gen-step-set sets symbols)
    (recur (gen-step-set sets symbols) symbols (dec n))))


(defn solve [symbols n]
  (if (not= n 0)
    (step (list(list)) symbols n)
    ()))

(println (solve (list "a" "b" "c") 5))