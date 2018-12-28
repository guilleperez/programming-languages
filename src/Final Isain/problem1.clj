;==========================================================
; Isain Cuadra Rivas A01375997
;==========================================================

(use 'clojure.test)

;==========================================================
(declare auxTriangular)

(defn triangular
  "Returns n if x is the n-th triangular number, otherwise
  returns nil."
  [x]
  (cond
    (zero? x)
    0

    :else
    (auxTriangular x 1)))


(defn auxTriangular
  "This function help the triangular function but this can take two arguments"
  [x n]
  (loop [x x
         n n]
    (cond
      (zero? (-' x n))
      n

      (> 0 (-' x n))
      nil

      :else
      (recur (-' x n) (inc n)))))


(defn auxTriangular2
  "This function help the triangular function but this can take two arguments"
  [x n]
  (cond
    (zero? (-' x n))
    n

    (> 0 (-' x n))
    nil

    :elseL
    (auxTriangular2 (-' x n) (inc n))))





;==========================================================
(deftest test-triangular
  (is (= 0 (triangular 0)))
  (is (= 1 (triangular 1)))
  (is (nil? (triangular 2)))
  (is (= 2 (triangular 3)))
  (is (nil? (triangular 4)))
  (is (nil? (triangular 5)))
  (is (= 3 (triangular 6)))
  (is (= 4 (triangular 10)))
  (is (= 10 (triangular 55)))
  (is (= 1000 (triangular 500500)))
  (is (nil? (triangular 500000)))
  (is (= 10000000 (triangular 50000005000000))))

;==========================================================
(run-tests)

