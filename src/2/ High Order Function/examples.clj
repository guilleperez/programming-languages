(use 'clojure.test)

(defn compose
  "Returns a new function that represents f(g(x))."
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (+ x 3))
(defn f2 [x] (* x x))
(def f3 (compose f1 f2))
(def f4 (compose f2 f1))
(def f5 (compose f3 f4))

(defn whatever
  [a b c d]
  (+ (* 3 a) (* 2 b) c (* d d)))

(defn whatever'
  [a]
  (fn [b]
    (fn [c]
      (fn [d]
        (+ (* 3 a) (* 2 b) c (* d d))))))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))

(defn fib
  "Return fibonacci "
  [n]
  (if (< n 2)
    n
    (+' (fib (- n 1)) (fib (- n 2)))))


(defn fib-loop
  "Return fibonacci with loop recur"
  [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b (+' a b) (inc i)))))

(defn duplicate
  "Duplicates every element in lst with recursion"
  [list]
  (if (empty? list)
    ()
    (cons (first list)
          (cons (first list)
                (duplicate (rest list))))))


(defn duplicate-loop
  "Duplicates every element in lst with loop recur"
  [list]
  (loop [list list
         result ()]
    (if (empty? list)
      (reverse result)
      (recur
        (rest list)
        (cons (first list) (cons (first list) result))))))


(defn compose
  "Returns a new function that represents f(g(x))"
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (+ x 3))
(defn f2 [x] (* x x))
(def f3 (compose f1 f2))
(def f4 (compose f2 f1))
(def f5 (compose f3 f4))

(defn my-map
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))


(run-tests)