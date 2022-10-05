(ns untitled.Lab13)

(defn my-reduce [f acc coll]                                ; слышал,что вы говорили кому-то это сделать
  (if (empty? (rest coll))
    (f acc (first coll))
    (f acc (my-reduce f (first coll) (rest coll)))))

(defn my-map [f coll] 
  (letfn [(func [acc x] (conj acc (f x)))]
    (reverse (my-reduce func nil coll))))

(defn my-filter [f coll]
  (letfn [(func [acc x] (if (f x) (conj acc x) acc))]
    (reverse (my-reduce func nil coll))))

(filter even? (range 15))
(my-filter even? (range 15))
;///////////////////////////////////////
(my-map dec '(3 4 6 75))
(map dec '(3 4 6 75))
(my-reduce + 5 '(1 2 3 4))
(reduce + 5 '(1 2 3 4))
(my-filter even? (range 7))
(filter even? (range 7))
(my-filter pos? (list 2 1 0 -4 -5 4 -4 2))
(filter pos? (list 2 1 0 -4 -5 4 -4 2))

