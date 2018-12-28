;==========================================================
; Type your student ID and name here.
;Guillermo Pérez Trueba
;A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn range-of-evens
  "Returns a list with every even integer i, where
  start <= i <= end. The resulting list is in ascending
  order."
  [start end]
  (loop [result ()
         x start]
    (cond
      (> start end) (reverse result)
      (and (= x end) (even? x)) (reverse (cons x result))
      (= x end) (reverse result)
      (even? x) (recur (cons x result) (inc x))
      :else (recur result (inc x))
    ))
  )

;==========================================================
(deftest test-range-of-evens
  (is (= ()
         (range-of-evens 10 0)))
  (is (= '(10)
         (range-of-evens 10 10)))
  (is (= '(-4 -2 0 2 4)
         (range-of-evens -4 5)))
  (is (= '(2 4 6 8 10)
         (range-of-evens 1 10)))
  (is (= '(-4 -2 0 2 4 6)
         (range-of-evens -5 7)))
  (is (= '(-6 -4 -2 0 2 4 6)
         (range-of-evens -6 7)))
  (is (= '(-6 -4 -2 0 2 4 6 8)
         (range-of-evens -6 8)))
  (is (= '(-50 -48 -46 -44 -42 -40 -38
            -36 -34 -32 -30 -28 -26 -24
            -22 -20 -18 -16 -14 -12 -10
            -8 -6 -4 -2 0 2 4 6 8 10 12
            14 16 18 20 22 24 26 28 30 32
            34 36 38 40 42 44 46 48 50)
         (range-of-evens -50 50))))

;==========================================================
(run-tests)
