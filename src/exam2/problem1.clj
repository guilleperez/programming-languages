;==========================================================
; Type your student ID and name here.
; Guillermo Pérez Trueba A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn parity-even
  "Returns a new list containing a partiy bit followed by 
  the seven original bits from lst. If the number of 1 bits
  in lst is an even number, the parity bit should be 0, 
  otherwise the parity bit should be 1."
  [lst]
  (if (= (rem (count (remove zero? lst)) 2) 0)
    (cons 0 lst) ;par
    (cons 1 lst)) ;non
  )

;(cons (rem (reduce + lst ) 2) lst))

;==========================================================
(deftest test-partity-even
  (is (= '(1 1 0 0 0 1 1 0)
         (parity-even '(1 0 0 0 1 1 0))))
  (is (= '(0 1 0 0 0 1 0 0)
         (parity-even '(1 0 0 0 1 0 0))))
  (is (= '(0 0 0 0 0 0 0 0)
         (parity-even '(0 0 0 0 0 0 0))))
  (is (= '(1 1 1 1 1 1 1 1)
         (parity-even '(1 1 1 1 1 1 1))))
  (is (= '(1 0 0 1 0 0 0 0)
         (parity-even '(0 0 1 0 0 0 0))))
  (is (= '(0 0 0 1 0 0 0 1)
         (parity-even '(0 0 1 0 0 0 1)))))

;==========================================================
(run-tests)
