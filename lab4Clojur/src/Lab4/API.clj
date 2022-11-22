(ns Lab4.API)
(defn constant-false? [expr]
  (= (first expr) :false)
  )

(defn constant-true? [expr]
  (= (first expr) :true)
  )

;проверка типа для константы
(defn constant? [expr]
  (or (constant-true? expr)
      (constant-false? expr)))

(defn variable? [expr]
  (= (first expr) :var))

(defn variable-name [v]
  (second v))

(defn same-variables? [v1 v2]
  (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1)
       (variable-name v2))))

; проверка типа левой скобки
(defn right-bracket? [expr]
  (= (first expr) :right-bracket))

; проверка типа правой скобки
(defn left-bracket? [expr]
  (= (first expr) :left-bracket))

; проверка типа для дизъюнкции
(defn conjunction? [expr]
  (= (first expr) :conjunction))

; проверка типа для дизъюнкции
(defn disjunction? [expr]
  (= (first expr) :disjunction))

; проверка типа для отрицания
(defn negation? [expr]
  (= (first expr) :negation))

; проверка типа для импликации
(defn implication? [expr]
  (= (first expr) :implication))

(def constant-true (list :true))
(def constant-false (list :false))

(defn constant [expr]
  (cond
    (constant-true? expr) constant-true
    (constant-false? expr) constant-false))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn variable-name [v]
  (second v))

(defn conjunction
  [expr & rest]
  (cons :conjunction (cons expr rest)))

(defn disjunction [expr & rest]
  (cons :disjunction (cons expr rest)))

(defn negation [expr & rest]
  (cons :negation (cons expr rest)))

(defn implication [expr & rest]
  (cons :implication (cons expr rest)))


(defn args [expr]
  (rest expr))