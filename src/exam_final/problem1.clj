;==========================================================
; Guillermo Pérez Trueba A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn diagonal-matrix?
  "Takes a matrix m (a vector of vectors) and returns
  true if it's a diagonal matrix, that is, the entries 
  outside the main diagonal are all zero, otherwise 
  returns false."
  [m]
  ;(let [n (count m)]
  ;  (let [mpct (mapcat #(concat % []) m)]
  ;            (into [] (cons (first mpct)(for [i (range 1 (count mpct))] ;(-  1))]
  ;      (if (zero? (rem i n))
  ;        (cons (nth mpct (+ 1 i)) [])
  ;       )))))))
  (let [n (count m)]
    (if (= (count m) 1)
      true
      (let [mpct (mapcat #(concat % []) m)]
        (if (>= (count (filter zero? mpct)) (- (* n n) n))
          true
          false)))))
;==========================================================
(deftest test-diagonal-matrix?
  (is (diagonal-matrix? []))
  (is (diagonal-matrix? [[5]]))
  (is (diagonal-matrix? [[5 0]
                         [0 6]]))
  (is (diagonal-matrix? [[4 0 0]
                         [0 2 0]
                         [0 0 7]]))
  (is (diagonal-matrix? [[1 0 0 0]
                         [0 2 0 0]
                         [0 0 3 0]
                         [0 0 0 4]]))
  (is (diagonal-matrix? [[0 0 0 0]
                         [0 0 0 0]
                         [0 0 0 0]
                         [0 0 0 0]]))
  (is (diagonal-matrix? [[0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 0 0]
                         [0 0 0 1 0]
                         [0 0 0 0 0]]))
  (is (diagonal-matrix? [[1 0 0 0 0 0 0 0]
                         [0 2 0 0 0 0 0 0]
                         [0 0 3 0 0 0 0 0]
                         [0 0 0 4 0 0 0 0]
                         [0 0 0 0 5 0 0 0]
                         [0 0 0 0 0 6 0 0]
                         [0 0 0 0 0 0 7 0]
                         [0 0 0 0 0 0 0 8]]))
  (is (not (diagonal-matrix? [[3 0 0]
                              [0 5 0]
                              [2 0 1]])))
  (is (not (diagonal-matrix? [[8 7 1 3 9]
                              [6 7 1 0 2]
                              [0 1 0 2 8]
                              [9 3 6 4 0]
                              [0 6 2 4 3]])))
  (is (not (diagonal-matrix? [[0 0 0 0 0]
                              [0 0 0 0 0]
                              [0 0 0 0 0]
                              [0 0 0 0 0]
                              [0 0 0 1 0]])))
  (is (not (diagonal-matrix? [[0 2 3 1 7 7 5 6 4 1]
                              [3 5 5 4 1 7 4 5 0 7]
                              [5 8 0 1 0 2 8 3 6 7]
                              [1 0 1 2 2 1 7 3 8 8]
                              [2 6 2 1 7 9 8 4 5 7]
                              [6 8 5 3 1 5 7 6 1 2]
                              [5 3 9 9 8 7 3 6 6 6]
                              [0 1 3 6 7 5 7 1 4 7]
                              [8 4 0 4 3 0 0 0 4 0]
                              [3 3 3 5 9 4 4 4 4 7]]))))

;==========================================================
(run-tests)
