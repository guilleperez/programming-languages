(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn suffixes
  "Returns a list with all the possible suffixes of lst"
  [lst]
  (take (inc (count lst))
  (iterate rest lst)))


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


(defn binary->decimal
  "Binario a decimal"
  [lst]
  ;(reduce +
  ;  (map *
  ;    (map #(expt 2 %)
  ;      (range (dec (count lst )) -1 -1))
  ;      lst)))


  ;(->> (range (dec (count lst)) -1 -1)
  ;     (map #(expt 2 %))
  ;     (map * lst)
  ;     (reduce +))

  (reduce (fn [accum digit] (+ digit (* 2 accum))) 0 lst)
  )



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
  ;range value stop step


(defn range-of-evens
  "Lista de pares"
  [start end]
  ;(filter even? (range start (inc end))))
  (->> (inc end)
       (range start)
       (filter even? )))

(deftest test-range-of-evens
  (is (= ()
         (range-of-evens 10 0)))
  (is (= '(10)
         (range-of-evens 10 10)))
  (is (= '(-4 -2 0 2 4)
         (range-of-evens -4 5)))
  (is (= '(2 4 6 8 10)
         (range-of-evens 1 10)))
  (is (= '(-4 -2 0 2 4 6)
         (range-of-evens -5 7)))
  (is (= '(-6 -4 -2 0 2 4 6)
         (range-of-evens -6 7)))
  (is (= '(-6 -4 -2 0 2 4 6 8)
         (range-of-evens -6 8)))
  (is (= '(-50 -48 -46 -44 -42 -40 -38
            -36 -34 -32 -30 -28 -26 -24
            -22 -20 -18 -16 -14 -12 -10
            -8 -6 -4 -2 0 2 4 6 8 10 12
            14 16 18 20 22 24 26 28 30 32
            34 36 38 40 42 44 46 48 50)
         (range-of-evens -50 50))))


(defn list-of-symbols?
  [lst]
  (every? symbol? lst )
  )

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))


(defn invert-pairs
  [lst]
  (map #(vector (second %) (first %) ) lst)
  )

(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([1 a][2 a][1 b][2 b]))
      (invert-pairs '([a 1][a 2][b 1][b 2])))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))


(defn pack
  [lst]
  (partition-by identity lst) ;identity returns the same value
  )

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

;The function encode takes a list lst as its argument.
; Consecutive duplicates of elements in lst are encoded as vectors [n e],
; where n is the number of duplicates of the element e. Unit tests:
;

(defn encode
  [lst]
  (map #(vector (count %) (first %)) (pack lst))
  )

;vec 1 parameter
;vector many parameters

(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))



(defn compress
  "The function compress takes a list lst as its argument.
  If lst contains consecutive\nrepeated elements, they should be replaced with a single copy of the element.
  The order of the elements should not be changed."
  [lst]
  (map first (pack lst)))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))


(defn encode-modified
  "The function encode-modified takes a list lst as its argument.
   It works the same as the previous problem, but if an element has no duplicates it
   is simply copied into the result list. Only elements with duplicates are converted to [n e] vectors."
  [lst]
  (map #(if (= (first %) 1)
          (second %)
          %)
       (encode lst))
  )

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

(defn decode
  "The function decode takes as its argument an encoded list lst
   that has the same structure as the resulting list from the previous problem.
   It returns the decoded version of lst. Unit tests:"
  [lst]
  (mapcat #(if (vector? %)
          (repeat (first %) (second %))
          (list %))
       lst)

  ; (reduce concat (map #(if (vector? %)
  ;    (repeat (first %) (second %))
  ;    (list %))
  ;  lst))
  )
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))


;(map # (* % %) (filter even? (range 10)))
;(->> (range 10) (filter even?) (map #(% %)))


;iterate [] body
(defn binary
  [n]
  ;(iterate f [n ()])
  (->> [n ()]
       (iterate (fn [[n result]]
                  [(quot n 2) (cons (rem n 2) result )]))
       (drop-while #(not (zero? (first %))))
       first
       second
       )
  )

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))


;n prime result
(defn prime-factors
  [n]
  (->> [n 2 ()] ;[n prime result] -> [ 100 2 ()]
       (iterate (fn [[n prime result]]  ;[[ parameters inseed a vector -> destrutor]]
                (if (zero? (rem n prime))
                  [(quot n prime) prime (cons prime result)] ;[50 2 ()]
                  ;(quot m n) is the value of m/n, rounded towards 0 to the nearest integer.
                  [n (inc prime) result])))
      (drop-while #(not= (first %) 1))
                first rest rest first reverse ;rest (rest ( first (100 2 ()))))
                ;(drop-wihle (f[[n _ _]] (not = n 1))
  ))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))


;for [ i '(4 -6 10 0 1)] (* i i))

;for [ i '(4 -6 10 0 1) :when (pos? i] (* i i)) == (map #(* % %) (filter pos? ') 4 -6 10 0 1)))
;for == map & filter

;cartersian product = (for [x '(a b c) y '(1 2 3)] [x y]) -> can add more variables

(for [a (range 1 100)
      b (range 1 100)
      c (range 1 100)
      :when (< a b c)
      :when (= (* c c) (+ (* a a) (* b b)))]
  [a b c])

(defn pow2
  [n]
  (lazy-seq (cons n (pow2 (*' n 2)))) ;' if result doesn't fit to a long, pass it to a BigInt
  )

(defn criba
  [lst]
  (lazy-seq (cons (first lst)
                  (criba (remove #(zero? (rem % (first lst))) lst)))
            ;filter -> se queda con los que cumplen
            ;remove -> se queda con los que no cumplan
    ))

;(drop-while #(< % 100) prime)

(def prime (criba (range 2 31)))
;(def prime (criba (iterate inc 2)))

(run-tests)



