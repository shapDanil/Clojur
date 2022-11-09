(ns untitled.Lab14)

(defn add-head [set symbols]                                ;переделано на map
  (letfn [(cur-cond [symbols] (not= (first set) symbols)) (add-map [cur-cond-symbols] (cons cur-cond-symbols set))]
    (map add-map (filter cur-cond symbols))))

(defn gen-step-set [sets symbols]                           ;map
  (letfn [(add-head-map [set] (add-head set symbols))]
    (apply concat (map add-head-map sets))))
(defn gen-step-set [sets symbols]                           ;mapcat
  (letfn [(add-head-map [set] (add-head set symbols))]
    (mapcat add-head-map sets)))

(defn step [sets symbols n]
  (if (= n 0)
    sets
    (step (gen-step-set sets symbols) symbols (dec n))))
(defn solve [symbols n]  ;iterate
  (if (>= n 0)
    (nth (iterate (fn [sets] (gen-step-set sets symbols)) (list(list))) n)
    (println "n>=0!!!")))
(defn solve [symbols n]
  (if (>= n 0)
    (step (list(list)) symbols n)
    ()))


(println (solve (list "a" "b" "c" "d") 2))





