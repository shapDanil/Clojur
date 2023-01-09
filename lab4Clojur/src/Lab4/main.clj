(ns Lab4.main
  (:require
    [Lab4.API :refer :all]
    [Lab4.DNF :refer :all]))

(defn -main []
  (println (signify (disjunction (variable :x) (variable :y)) (variable :x) constant-true)))

