;==========================================================
; Type your student ID and name here.
;Guillermo Pérez Trueba
;A01377162
;==========================================================

(use 'clojure.test)
(use 'clojure.math.numeric-tower)

;==========================================================
(defn binary->decimal
  "Returns the equivalent decimal value of the series
  of binary digits contained in lst."
  [lst]
  (loop [lst (reverse lst)
         result 0
         i 0]
    (cond
      (empty? lst) result
      (= 0 (first lst)) (recur (rest lst) result (inc i))
      (= 1 (first lst)) (recur (rest lst) (+ result (expt 2 i)) (inc i))


      )
    )
  )

;==========================================================
(deftest test-binary->decimal
  (is (= 0
         (binary->decimal ())))
  (is (= 1
         (binary->decimal '(1))))
  (is (= 2
         (binary->decimal '(1 0))))
  (is (= 5
         (binary->decimal '(1 0 1))))
  (is (= 8
         (binary->decimal '(1 0 0 0))))
  (is (= 42
         (binary->decimal '(1 0 1 0 1 0))))
  (is (= 63
         (binary->decimal '(1 1 1 1 1 1))))
  (is (= 24601
         (binary->decimal '(1 1 0 0 0 0 0 0 0 0 1 1 0 0 1)))))

;==========================================================
(run-tests)
