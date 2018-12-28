;==========================================================
; Type your student ID and name here.
;Guillermo Pérez Trueba
;A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn suffixes
  "Returns a list with all the possible suffixes of lst."
  [lst]
  (loop [lst lst
         index 0
         restul ()]
      (cond
        (empty? lst) (reverse (conj restul lst))
        :else (recur (rest lst) (inc index) (conj restul lst))
    )))

;==========================================================
(deftest test-suffixes
  (is (= '(())
         (suffixes ())))
  (is (= '((a) ())
         (suffixes '(a))))
  (is (= '((a b) (b) ())
         (suffixes '(a b))))
  (is (= '((a b c d) (b c d) (c d) (d) ())
         (suffixes '(a b c d))))
  (is (= '((a b c d e f g) (b c d e f g)
            (c d e f g) (d e f g)
            (e f g) (f g)
            (g) ())
         (suffixes '(a b c d e f g)))))

;==========================================================
(run-tests)
