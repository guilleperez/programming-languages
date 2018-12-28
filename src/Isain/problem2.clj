;==========================================================
; A01375997 Isain Cuadra Rivas
;==========================================================

(use 'clojure.test)

;==========================================================
(defn every-insert
  "Returns a new list with all the possible ways in which 
  x can be inserted into every position of lst."
  [x lst]
  (map #(concat (first %) (list x) (second %)) (for
                                                 [i (range (inc (count lst)))]
                                                 (split-at i lst)))
  )


;==========================================================
(deftest test-every-insert
  (is (= '((1)) (every-insert 1 ())))
  (is (= '((1 a) (a 1)) (every-insert 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (every-insert 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (every-insert 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (every-insert 'x '(1 2 3 4 5 6 7 8 9 10)))))

;==========================================================
(run-tests)