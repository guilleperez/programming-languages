;==========================================================
; Type your student ID and name here.
; Guillermo Pérez Trueba A01377162
;==========================================================

(use 'clojure.test)

;==========================================================
(defn look-and-say
  "Returns the n-th element of the look-and-say sequence
  as a list of digits."
  [x]
  (->> [0 '(1)]
       (iterate (fn [[n v]] [(inc n)(mapcat #(vector (count %) (first %)) (partition-by identity v))]))
       (drop-while #(not= (first %) x))
                  first last))
  ;(first
  ; (last
  ;   (take x
  ;     (->> ['(1)]
  ;       iterate (fn [[arr]] [(mapcat #(vector (count %) (first %)) (partition-by identity arr))]))))))

;==========================================================
(deftest test-look-and-say
  (is (= '(1) (look-and-say 0)))
  (is (= '(1 1) (look-and-say 1)))
  (is (= '(2 1) (look-and-say 2)))
  (is (= '(1 2 1 1) (look-and-say 3)))
  (is (= '(1 1 1 2 2 1) (look-and-say 4)))
  (is (= '(3 1 2 2 1 1) (look-and-say 5)))
  (is (= '(3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2
            3 1 1 3 3 2 1 1 1 2 1 3 1 2 2 1
            1 2 3 1 1 3 1 1 1 2 3 1 1 2 1 1
            1 3 3 1 1 2 1 1 1 3 1 2 2 1 1 2
            1 3 2 1 1 3 1 2 1 1 1 3 2 2 2 1
            1 2 3 1 1 3 1 1 2 2 2 1 1 3 1 1
            1 2 2 1 2 2 1 1 1 3 1 2 2 1 1 2
            1 3 2 1 1 3 1 2 1 1 1 3 2 2 2 1
            1 2 1 3 2 1 1 3 2 1 3 2 2 1 1 3
            3 1 1 2 1 3 2 1 2 3 2 2 2 1 1 2
            3 1 1 3 1 1 2 2 2 1 1 3 1 1 1 2
            3 1 1 3 2 2 3 1 1 2 1 1 1 3 1 1
            2 2 2 1 1 2 1 3 2 1 1 3 3 1 1 2
            1 3 2 1 1 2 2 1 1 2 1 3 3 2 2 1
            1 2 1 1 1 3 1 2 2 1 1 3 1 2 1 1
            1 3 2 2 2 1 2 3 2 1 1 2 1 1 1 3
            1 2 1 1 1 2 1 3 1 1 1 2 1 3 2 1
            1 2 3 1 1 3 2 1 3 2 2 1 1 2 1 1
            1 3 1 2 2 1 2 3 2 1 1 2 1 1 1 3
            1 2 2 1 1 2 1 3 1 1 1 2 1 3 1 2
            2 1 1 2 1 3 2 1 1 3 2 1 3 2 2 1
            1 2 3 1 1 3 1 1 2 2 2 1 1 3 1 1
            1 2 3 1 1 3 1 1 1 2 1 3 2 1 1 2
            2 1 1 2 1 3 2 2 3 1 1 2 1 1 1 3
            1 2 2 1 1 3 3 2 2 1 1 3 1 1 1 2
            2 1 1 3 1 2 2 1)
         (look-and-say 20))))

;==========================================================
(run-tests)
