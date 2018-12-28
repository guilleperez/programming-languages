;==========================================================
; Guillermo Perez Trueba A01377162
;==========================================================

(use 'clojure.test)
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;==========================================================
(l/defne inserto
  "Logic function that succeeds if result is equal to lst
  (which must be a list of numbers in ascending order) with 
  number x inserted in the correct position. In other words, 
  the ascending order of the elements in the resulting 
  sequence is preserved."
  [x list result]
         ([x list result]
           (l/fresh [temp head tail]
                    (fd/>= x head)
                    (l/conso head tail list)
                    (l/appendo [head] list result)))

         ([x list result]
           (l/fresh [temp head tail]
                    (l/conso head tail list)
                    (fd/< x head)
                    (l/appendo [head] temp result)
                    (inserto x list result)))
         )

;==========================================================
(deftest test-inserto
  (is (= [[5]]
         (l/run 1 [q]
           (inserto 5 [] q))))
  (is (= [:yes]
         (l/run 1 [q]
           (inserto 3 [1 2 4 5] [1 2 3 4 5])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (inserto 3 [1 2 3 4 5] [1 2 3 3 4 5])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (inserto 3 [1 2 3 4 5] [1 2 3 4 5])
           (l/== q :yes))))
  (is (= [[1 2 3 4 5]]
         (l/run 1 [q]
           (inserto 1 [2 3 4 5] q))))
  (is (= [[4 8 15 16 23 42]]
         (l/run 1 [q]
           (inserto 15 [4 8 16 23 42] q))))
  (is (= [[4 8 15 16 23 42]]
         (l/run 1 [q]
           (inserto 42 [4 8 15 16 23] q))))
  (is (= [23]
         (l/run 1 [q] 
           (inserto q [4 8 15 16 42] [4 8 15 16 23 42]))))
  (is (= [[4 8 15 16 42]]
         (l/run 1 [q]
           (inserto 23 q [4 8 15 16 23 42]))))
  (is (= [[4  [8 15 16 23 42]] 
          [8  [4 15 16 23 42]] 
          [15 [4 8 16 23 42]] 
          [16 [4 8 15 23 42]] 
          [23 [4 8 15 16 42]] 
          [42 [4 8 15 16 23]]]
         (l/run* [p q] 
           (inserto p q [4 8 15 16 23 42])))))

;==========================================================
(run-tests)
