;==========================================================
; Isain Cuadra Rivas A01375997
;==========================================================

(use '[clojure.test :rename {is test-is}])
(use '[clojure.core.logic :rename {== ===}])
(require '[clojure.core.logic.fd :as fd])

;==========================================================
(defne facto
  "Logical function that succeeds if the factorial of}
   n is result."
  [n result]

  ([0 result])

  ([1 result])

  ([n result]
   (facto (dec n) (* n (dec n)))))













;#(apply * (range 1 (inc %)))


(deftest test-facto
  (test-is (= [1]
              (run 1 [q] (facto 0 q))))
  (test-is (= [1]
              (run 1 [q] (facto 1 q))))
  (test-is (= [720]
              (run 1 [q] (facto 6 q))))
  (test-is (= [2432902008176640000]
              (run 1 [q] (facto 20 q))))
  (test-is (= [0 1]
              (run 2 [q] (facto q 1))))
  (test-is (= [5]
              (run 1 [q] (facto q 120))))
  (test-is (= [10]
              (run 1 [q] (facto q 3628800))))
  (test-is (= [:yes]
              (run 1 [q] (facto 4 24)
                         (=== q :yes))))
  (test-is (= [:yes]
              (run 1 [q] (facto 15 1307674368000)
                         (=== q :yes))))
  (test-is (= [[0 1]
               [1 1]
               [2 2]
               [3 6]
               [4 24]
               [5 120]
               [6 720]
               [7 5040]
               [8 40320]
               [9 362880]]
              (run 10 [n r] (facto n r)))))

;==========================================================
(run-tests)

