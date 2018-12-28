;==========================================================
; Isain Cuadra Rivas A01375997
;==========================================================

(use 'clojure.test)

;==========================================================
(defmacro nth-expr
  "Evaluates only the nth provided expression, the
  rest are ignored."
  [nth & exprs]
  `(let [n# ~nth]
     (println n#)
     (println ~exprs)
     ;(nth n# ~@exprs)
     )
  )






(deftest test-nth-expr
  (is (= '(clojure.core/case (- 5 4)
            0 (* 2 3)
            1 (- 5 2)
            2 (+ 7 2)
            3 (/ 20 2)
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- 5 4)
                                   (* 2 3)
                                   (- 5 2)
                                   (+ 7 2)
                                   (/ 20 2)))))
  (is (= '(clojure.core/case (- (* 2 2) (+ 2 2))
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- (* 2 2) (+ 2 2))))))
  (is (= '(clojure.core/case (- (* 2 3) (+ 2 3))
            0 (println "zero")
            1 (println "one")
            2 (println "two")
            3 (println "three")
            4 (println "four")
            5 (println "five")
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- (* 2 3) (+ 2 3))
                                   (println "zero")
                                   (println "one")
                                   (println "two")
                                   (println "three")
                                   (println "four")
                                   (println "five")))))
  (is (= 3 (nth-expr (- 5 4)
                     (* 2 3)
                     (- 5 2)
                     (+ 7 2)
                     (/ 20 2))))
  (is (thrown?
        RuntimeException
        (nth-expr (- (* 2 2) (+ 2 2)))))
  (is (thrown?
        RuntimeException
        (nth-expr :wat 0 1 2 3 4 5)))
  (is (= "one\n"
         (with-out-str (nth-expr (- (* 2 3) (+ 2 3))
                                 (println "zero")
                                 (println "one")
                                 (println "two")
                                 (println "three")
                                 (println "four")
                                 (println "five")))))
  (is (thrown?
        RuntimeException
        (nth-expr (* (* 2 3) (+ 2 3))
                  (println "zero")
                  (println "one")
                  (println "two")
                  (println "three")
                  (println "four")
                  (println "five")))))

;==========================================================
(run-tests)




