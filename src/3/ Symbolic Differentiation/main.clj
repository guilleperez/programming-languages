;(+ augend addend)


(defn variable?
      "Is e variable?"
      [e]
      (symbol? e))

(defn same-variable?
  "Are v1 and v2 the same variable?"
  [v1 v2]
  (and (variable? v1) (variable? v2)
       (= v1 v2)))

(defn sum?
  "Is e a sum?"
  [e]
  (and (list? e) (= 3 (count e)) (= '+ (first e))))

(defn augend
  "Augend of the sum e."
  [e]
  ;(firs (rest e))
  (nth e 1) ;index = 1
  )

(defn addend
  "Addend  of the sum e."
  [e]
  ;(firs (rest e))
  (nth e 2) ;index = 2
  )

(defn make-sum
  "Construct the sum of a1 and a2."
  [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1) (number? a1)) (+ a1 a2)
    :else (list '+ a1 a2)
  ))

(defn product?
  "Is e a product?"
  [e]
  (and (list? e) (= 3 (count e)) (= '* (first e))))


(defn multiplier
  "Multiplier of the product e."
  [e]
  (addend e)
  ;(nth e 2) ;index = 2
  )




(defn multiplicand
   "Multiplicand of the product e."
   [e]
    ;u // v
   (augend e)
    ;index = 3
)


(defn make-product
 "Construct the product of m1 and m2."
  [a1 a2]
 (cond
    (= a1 0) 0
    (= a2 0) 0
    (= a1 1) a2
    (= a2 1) a1
    (and (number? a1) (number? a2)) (* a1 a2)
    :else (list '* a1 a2)
 ))


(defn deriv
  "Take as arguments an algebraic expression exp and a variable var
  and to return the derivative of the expression with respect to the variable."
  [exp var]
   (cond
      (number? exp) 0 ;d cdx
      (variable? exp) ;d xdx
      (if (same-variable? exp var) 1 0)
      (sum? exp) (make-sum (deriv (augend exp) var) (deriv (addend exp) var)) ;du + dv dx
      (product? exp) (make-sum
                       (make-product (multiplier  exp) (deriv (multiplicand  exp) var))
                       (make-product (multiplicand  exp) (deriv (multiplier  exp) var))
                       ) ;d (u+v)dx
   ))