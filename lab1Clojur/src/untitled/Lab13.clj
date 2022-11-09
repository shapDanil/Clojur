(ns untitled.Lab13)


; если использовать так то при больших последовательностей будет переполнение стека
(defn my-reduce [f acc coll]           ; слышал,что вы говорили кому-то это сделать
  (if (empty? (rest coll))
    (f acc (first coll))
    (my-reduce f (f acc (first coll)) (rest coll))))
; так должна решаться проблема выше
(defn my-reduce [func acc coll]
  (if (empty? coll)
    acc
    (recur func (func acc (first coll)) (rest coll))))


(defn my-map [f coll]   ;// без реверса
  (letfn [(func [acc x] (concat acc (list (f x))))]
    (my-reduce func nil coll)))
(defn my-map [f coll]
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (my-reduce func nil coll))))
(defn my-filter [f coll]  ;// без реверса
  (letfn [(func [acc x] (concat acc (if (f x) (list x))))]
    (my-reduce func nil coll)))
(defn my-filter [f coll]
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (my-reduce func nil coll))))

(filter even? (range 15))
(my-filter even? (range 15))
;///////////////////////////////////////
(println(my-map dec '(3 4 6 75)))
(println(map dec '(3 4 6 75)))
(println(my-reduce + 5 '(1 2 3 4)))
(println(reduce + 5 '(1 2 3 4)))
(println(my-filter even? (range 7)))
(println(filter even? (range 7)))
(println(my-filter pos? (list 2 1 0 -4 -5 4 -4 2)))
(println(filter pos? (list 2 1 0 -4 -5 4 -4 2)))

