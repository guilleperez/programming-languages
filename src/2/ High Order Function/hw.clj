;----------------------------------------------------------
; Activity: Problem Set: High Order Functions
; Date: September 9, 2018.
; Authors:
;          A01377515 Pablo Alejandro Sánchez Tadeo
;          A01377162 Guillermo Perez Trueba
;----------------------------------------------------------
(use 'clojure.test)

(use 'clojure.math.numeric-tower)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

; 1
(defn my-map-indexed
  "Takes two arguments: function f and a list lst. It returns a list consisting of the result of
  applying f to 0 and the first item of lst, followed by applying f to 1 and the second item in lst,
  and so on until lst is exhausted. Function f should accept 2 arguments: index and item."
  [f lst]
  (loop [result ()
         lst lst
         index 0]

    (if (empty? lst)
      (reverse result)
      (recur (cons (f index (first lst)) result) (rest lst) (inc index)))))

(deftest test-my-map-indexed
  (is (= () (my-map-indexed vector ())))
  (is (= '([0 a] [1 b] [2 c] [3 d])
         (my-map-indexed vector '(a b c d))))
  (is (= '(10 4 -2 8 5 5 13)
         (my-map-indexed + '(10 3 -4 5 1 0 7))))
  (is (= '(0 1 -4 3 1 0 6)
         (my-map-indexed min '(10 3 -4 5 1 0 7)))))

;3
(defn bisection
  "Takes a, b, and f as arguments. It finds the corresponding root using the bisection method.
  The algorithm must stop when a value of c is found such that: |f(c)| < 1.0×10-15."
  [a b f]
  (loop [a  a
         b  b]
  (let [c (/ (+ a b) 2)]
  (cond
    (< (abs (f c)) 0.00000000000001) (float c)
    (and (< (f a) 0) (> (f c) 0)) (recur a c)
    (and (> (f a) 0) (< (f c) 0)) (recur a c)
    (and (< (f b) 0) (> (f c) 0)) (recur c b)
    (and (> (f b) 0) (< (f c) 0)) (recur c b)
    ))))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
 (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1)))))
)

;5
(defn integral
  "Takes as arguments a, b, n, and f. It returns the value of the integral, using Simpson's rule."
  [a b n f]
  (loop[x (/ (- b a) n)
        index 0
        simpson 0]
    (cond
      (= index 0) (recur x (inc index) (f a))
      (= index n) (recur x (inc index) (+ simpson (f b)))
      (= index (+ n 1))  (* (/ x 3) simpson)
      (odd? index) (recur x (inc index) (+ simpson (* 4 (f (+ a (* index x))))))
      (even? index) (recur x (inc index) (+ simpson (* 2 (f (+ a (* index x))))))
      ))

)

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4 (integral 1 2 10 (fn [x] (integral 3 4 10 (fn [y] (* x y))))))))

(run-tests)
