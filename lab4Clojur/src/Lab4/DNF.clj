(ns Lab4.DNF
  (:require
    [Lab4.API :refer :all]))
;из лекции
(defn apply-rules [expr rules]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         rules)
   expr))

; --------------------------------------------------
(declare remove-not-standard-functions)
(def remove-not-standard-functions-rules
  (list
    ; x -> y = x + y
    [(fn [expr] (implication? expr))
     (fn [expr] (remove-not-standard-functions
                  (disjunction
                    (negation (remove-not-standard-functions (first (args expr))))
                    (remove-not-standard-functions (second (args expr))))))]
    ; x * y
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map remove-not-standard-functions (args expr))))]
    ; x + y
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map remove-not-standard-functions (args expr))))]
    ;!x
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (remove-not-standard-functions (second expr))))]

    ; true false
    [(fn [expr] (constant? expr))
     (fn [expr] expr)
     ]
    ; variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]))

(declare remove-not-standard-functions)
(defn remove-not-standard-functions [expr]
  (apply-rules expr remove-not-standard-functions-rules))
; ---------------------------------------------------------------------

(declare work-with-negation)
(def work-with-negation-rules
  (list
    ; !(x*y......) = !x + !y +....
    [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
     (fn [expr] (work-with-negation (apply disjunction (map #(negation %) (args (second expr))))))]
    ; !(x+y+...) = !x * !y * ....
    [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
     (fn [expr] (work-with-negation (apply conjunction (map #(negation %) (args (second expr))))))]
    ; !!x = x
    [(fn [expr] (and (negation? expr) (negation? (second expr))))
     (fn [expr] (work-with-negation (first (args (second expr)))))]
    ; !1
    [(fn [expr] (and (negation? expr) (constant-true? (first (args expr)))))
     (fn [expr] constant-false)]
    ; !0
    [(fn [expr] (and (negation? expr) (constant-false? (first (args expr)))))
     (fn [expr] constant-true)]
    ; x * y
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map work-with-negation (args expr))))
     ]
    ; x + y
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map work-with-negation (args expr))))
     ]
    ;!x
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (work-with-negation (second expr))))]

    ; true false
    [(fn [expr] (constant? expr))
     (fn [expr] expr)
     ]
    ; variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]))

(declare work-with-negation)
(defn work-with-negation [expr]
  (apply-rules expr work-with-negation-rules)
  )


(declare apply-distributivity)
(def apply-distributivity-rules
  (list
    ; x * (y + z) = (x * y) + (x * z)
    [(fn [expr] (and (conjunction? expr) (disjunction? (nth expr 2))))
     (fn [expr]
       (apply-distributivity (disjunction
                               (conjunction
                                 (first (args expr))
                                 (first (args (second (args expr))))
                                 )
                               (conjunction
                                 (first (args expr))
                                 (second (args (second (args expr))))
                                 )))
       )
     ]
    ; (y + z) * x = (y * z) + (z * x)
    [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
     (fn [expr]
       (apply-distributivity (disjunction
                               (conjunction (first (args (first (args expr))))
                                            (second (args expr)))
                               (conjunction (second (args (first (args expr))))
                                            (second (args expr))))))]
    ; x * y
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map apply-distributivity (args expr))))]
    ; x + y
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map apply-distributivity (args expr))))
     ]
    ;!x
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (apply-distributivity (second expr))))]

    ; true false
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]
    ; variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]))

(defn apply-distributivity[expr]
  (apply-rules expr apply-distributivity-rules))


(declare unite-disjunction-conjunction)
(def unite-disjunction-conjunction-rules
  (list
    [(fn [expr] (and (conjunction? expr) (some conjunction? (args expr))))
     (fn [expr] (let [conj #(some (if (conjunction? %) % nil) (args expr))
                      conj-args (args conj)]

                  (unite-disjunction-conjunction
                    (apply conjunction
                           (concat (remove #(= % conj) (args expr))
                                   conj-args)))))]

    [(fn [expr] (and (disjunction? expr) (some disjunction? (args expr))))
     (fn [expr] (let [disj (some #(if (disjunction? %) % nil) (args expr))
                      disj-args (args disj)]

                  (unite-disjunction-conjunction
                    (apply disjunction
                           (concat (remove #(= % disj) (args expr)) disj-args)))))]
    ; x * y
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map unite-disjunction-conjunction (args expr))))]
    ; x + y
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map unite-disjunction-conjunction (args expr))))]
    ;!x
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (unite-disjunction-conjunction (second expr))))]

    ; true false
    [(fn [expr] (constant? expr))
     (fn [expr] expr)]
    ; variable
    [(fn [expr] (variable? expr))
     (fn [expr] expr)]))

(declare unite-disjunction-conjunction)
(defn unite-disjunction-conjunction[expr]
  (apply-rules expr unite-disjunction-conjunction-rules)
  )

(declare remove-some-conj-disj-with-constants)
(def remove-some-conj-disj-with-constants-rules (list
                                                  ; x * 1 = x * ; x * 0 = 0
                                                  [(fn [expr] (and (conjunction? expr) (some constant? (args expr))))
                                                   (fn [expr] (if (= (some #(when (constant? %) %) (args expr)) constant-false)
                                                                  constant-false
                                                                  (apply conjunction (remove #(= % constant-true) (args expr)))))]
                                                  ; x + 1 = 1; x + 0 = x &
                                                  [(fn [expr] (and (disjunction? expr) (some constant? (args expr))))
                                                   (fn [expr] (if (= (some #(when (constant? %) %) (args expr)) constant-true)
                                                                constant-true
                                                                (apply disjunction (map remove-some-conj-disj-with-constants (remove #(= % constant-false) (args expr))))))]
                                                  [(fn [expr] (conjunction? expr)) (fn [expr] expr)]
                                                  [(fn [expr] (disjunction? expr))
                                                   (fn [expr] (apply disjunction (map remove-some-conj-disj-with-constants (args expr))))]
                                                  [(fn [expr] (variable? expr))
                                                   (fn [expr] expr)]
                                                  [(fn [expr] (and (negation? expr) (variable? (first (args expr)))))
                                                   (fn [expr] expr)]
                                                  [(fn [expr] (constant? expr))
                                                   (fn [expr] expr)]))
(defn remove-some-conj-disj-with-constants [expr]
  (apply-rules expr remove-some-conj-disj-with-constants-rules))


(defn remove-single-disjunction
  [expr]
  (if (disjunction? expr)
    (if (> (count (args expr)) 1)
      expr
      (second expr))
    expr))

(defn remove-single-conjunction
  [expr]
  (if (conjunction? expr)
    (if (> (count (args expr)) 1)
      expr
      (second expr))
    expr))

(defn remove-negation-variable-or-constant
  [x]
  {:pre [(or (variable? x) (constant? x) (and (negation? x) (or (variable? (first (args x))) (constant? (first (args x))))))]}
  (if (or (variable? x) (constant? x))
    x
    (first (args x))))

(defn remove-contradictions-conjunction [expr]
  (let [args-list (args expr)
        no-negations-list (map (fn [elem] (remove-negation-variable-or-constant elem)) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-false
      expr)))

(defn remove-contradictions-disjunction [expr]
  (let [args-list (args expr)
        no-negations-list (map #(if (negation? %) (second %) %) args-list)]
    (if (< (count (distinct no-negations-list)) (count (distinct args-list)))
      constant-true
      (distinct expr))))

(defn simplify-conjunction [expr]
  (let [result (if (or (constant? expr) (variable? expr) (and (negation? expr) (variable? (first (args expr)))))
                 expr
                 (remove-contradictions-conjunction (apply conjunction (distinct (args expr)))))]
    (remove-single-conjunction result)))

(defn simplify-disjunction [expr]
  (->>
    (apply disjunction
           (map simplify-conjunction
                (if  (or (constant? expr) (variable? expr) (and (negation? expr) (variable? (first (args expr)))))
                  expr
                  (args expr))))
    remove-contradictions-disjunction))



(defn simplify
  [expr]
  (let [result (cond
                 (constant? expr) expr
                 (variable? expr) expr
                 (negation? expr) expr
                 (conjunction? expr) (simplify-conjunction expr)
                 (disjunction? expr) (simplify-disjunction expr))]
    (remove-single-disjunction result)))

(defn dnf [expr]
  (->>
    expr
    remove-not-standard-functions
    work-with-negation
    apply-distributivity
    unite-disjunction-conjunction
    simplify
    remove-some-conj-disj-with-constants
    simplify))

;****************************************
(declare signify-expression)
(declare signify-rules)
(defn signify-expression [var val expr]
  (apply-rules expr (signify-rules var val)))

(defn signify-rules [var val]
  (list
    [(fn [expr] (and (variable? expr) (same-variables? var expr)))
     (fn [expr] val)]
    [(fn [expr] (conjunction? expr))
     (fn [expr] (apply conjunction (map #(signify-expression var val %) (args expr))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr] (apply disjunction (map #(signify-expression var val %) (args expr))))]
    [(fn [expr] (negation? expr))
     (fn [expr] (negation (signify-expression var val (second expr))))]
    [(fn [expr] (or (variable? expr) (constant? expr)))
     (fn [expr] expr)]))



(defn  signify [expr var val]
  (->>
    expr
    dnf
    (signify-expression var val)
    dnf))

