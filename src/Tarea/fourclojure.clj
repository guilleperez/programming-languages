(use 'clojure.test)
(use 'clojure.set)

(defn get-last
  [lst]
  (if (empty? (rest lst))
    (first lst)
    (recur (rest lst))))

(deftest test-integral
  (is (= (get-last [1 2 3 4 5]) 5)))

;Penultimate Element
(defn penultimate
  "Write a function which returns the second to last element from a sequence."
  [lst]
  (second (reverse lst)))

(deftest test-penultimate
  (is (= (penultimate (list 1 2 3 4 5)) 4) )
  (is (= (penultimate ["a" "b" "c"]) "b")))

;Nth Element
(defn nth-element
  "Write a function which returns the Nth element from a sequence."
  [lst index]
  (loop [i 0
         index index
         lst lst]
    (if (= i index)
          (first lst)
    (recur (inc i) index (rest lst)))))

(deftest test-nth
  (is (= (nth-element '(4 5 6 7) 2) 6)))

;Count a Sequence
(defn count-elements
  "Write a function which returns the total number of elements in a sequence."
  [lst]
  (loop [count 0
         lst lst]
    (if (empty? lst)
      count
      (recur (inc count) (rest lst)))))

(deftest test-count-elements
  (is (= (count-elements  '(1 2 3 3 1)) 5))
  (is (= (count-elements "Hello World") 11)))

;Sum It All Up
(defn suma
  "Write a function which returns the sum of a sequence of numbers."
  [lst]
  (reduce + lst))

(deftest test-suma
  (is (= (suma  [1 2 3]) 6)))

;Find the odd numbers
(defn odd
  "Write a function which returns only the odd numbers from a sequence."
  [lst]
  (filter #(odd? %) lst))

(deftest test-odd
  (is (= (odd [4 2 1 6]) '(1))))

;Reverse a Sequence
(defn reverses
  "Write a function which reverses a sequence."
  [lst]
  (loop [lst lst
         result ()]
    (if (empty? lst)
      result
      (recur (rest lst) (cons (first lst) result))
      )))

(deftest test-reverses
  (is (= (reverses [1 2 3 4 5]) [5 4 3 2 1])))

;Palindrome Detector
(defn palindrome
  "Write a function which returns true if the given sequence is a palindrome."
  [lst]
  (if (string? lst)
    (if (= (apply str (reverse lst)) lst)
      true
      false)
    (if (= (reverse lst) lst)
      true
      false)))

(deftest test-palindrome
  (is (= (false? (palindrome '(1 2 3 4 5)))))
  (is (= (true? (palindrome "racecar")))))

;Fibonacci Sequence
(defn fibonacci
  "Write a function which returns the first X fibonacci numbers."
  [x]
  (->> [2 '(1 1)]
       (iterate (fn [[n result]]
                  [(inc n) (cons  (+ (second result) (first result)) result) ]))
       (drop-while #(not= (first %) x))
       first second reverse))

(deftest test-fibonacci
  (is (= (fibonacci 3) '(1 1 2))))

;A Half-Truth
(defn bool
  "Return true if some of the parameters are true,
  but not all of the parameters are true. Otherwise your function should return false."
  [& args]
  (true? (and (some true? args) (not (every? true? args)))))

(deftest test-bool
  (is (= true (bool true true true false)))
  (is (= false (bool true true)))
  (is (= false (bool false false))))

;Maximum Value
(defn maximum
  "Write a function which takes a variable number of parameters and returns the maximum value."
  [& args]
  (last (sort args)))

(deftest test-maximum
  (is (= (maximum 45 67 11) 67)))

;Get the Caps
(defn caps
  "Write a function which takes a string and returns a new string containing only the capital letters."
  [txt]
  (apply str (re-seq #"[A-Z]" txt)))

(deftest test-caps
  (is (= (caps "HeLlO, WoRlD!") "HLOWRD")))

;Duplicate a Sequence
(defn dup
  "Write a function which duplicates each element of a sequence."
  [lst]
  (mapcat #(repeat 2 %) lst))

(deftest test-dup
  (is (= (dup [1 2 3]) '(1 1 2 2 3 3))))

;Intro to some

;(defn some?
;  [f col]
;  "Returns the first logical true value of (predicate x) where x is an item in the collection.\ntest not run"
;  )

;Implement Range
(defn rng
  "Write a function which creates a list of all integers in a given range."
  [first last]
  (take (- last first) (iterate #(+ % 1) first)))

(deftest test-rng
  (is (= (rng 1 4) '(1 2 3)))
  (is (= (rng -2 2) '(-2 -1 0 1))))

;Compress a sequence
(defn compress
  "Write a function which removes consecutive duplicates from a sequence."
  [lst]
  (map first (partition-by identity lst)))

(deftest test-compress
  (is (= (compress [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))))

;Factorial Fun
(defn factorial
  [n]
  "Write a function which calculates factorials."
  (reduce * (take n (iterate #(+ % 1) 1))))

(deftest test-factorial
  (is (= (factorial 3) 6)))

;Interleave
(defn inter
  "Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc."
  [a b]
  (loop [a a
         b b
         result ()]
    (cond
      (empty? a) (reverse result)
      (empty? b) (reverse result)
      :else (recur (rest a) (rest b)  (cons (first b) (cons (first a) result))))))

(deftest test-inter
  (is (= (inter [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))))

;Flatten a sequence
(defn my-flatten
  "Write a function which flattens a sequence."
  [lst]
  (if (empty? lst)
    lst
    (if (or (list? (first lst)) (vector? (first lst)))
      (concat (my-flatten (first lst)) (my-flatten (rest lst)))
      (cons (first lst) (my-flatten (rest lst))))))

(deftest test-my-flatten
  (is (= (my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))))

;Replicate a Sequence
(defn replicates
  [lst n]
  "Write a function which replicates each element of a sequence a variable number of times."
  (mapcat #(repeat n %) lst))

(deftest test-replicates
  (is (= (replicates [1 2 3] 2) '(1 1 2 2 3 3))))

;Interpose a Seq
(defn my-interpose
  [n lst]
  "Write a function which separates the items of a sequence by an arbitrary value."
  (butlast (mapcat #(reverse (cons n %)) (partition 1 lst))))


(deftest test-my-interpose
  (is (= (my-interpose 0 [1 2 3]) [1 0 2 0 3])))

;Pack a Sequence
(defn pack
  "Write a function which packs consecutive duplicates into sub-lists."
  [lst]
  (partition-by identity lst))

(deftest test-pack
  (is (= (pack [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))))

;Drop Every Nth Element
(defn drop-nth
  "Write a function which drops every Nth item from a sequence."
  [lst n]
  (mapcat #(if (= n (count %)) (butlast %) %) (partition-all n lst)))

(deftest test-drop-nth
  (is (= (drop-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])))

;Split a sequence
(defn split
  "Write a function which will split a sequence into two parts."
  [n lst]
  (conj (list (drop n lst)) (take n lst)))

(deftest test-split
  (is (= (split 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split 1 [:a :b :c :d]) [[:a] [:b :c :d]])))

;Map Construction
(defn my-map
  "Write a function which takes a vector of keys and a vector of values and constructs a map from them."
  [keys values]
  (apply assoc {} (interleave keys values)))

(deftest test-my-map
  (is (= (my-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})))

;Greates Common Divisor
(defn gcm
  "Given two integers, write a function which returns the greatest common divisor."
  [a b]
  (first (last (filter #(> (count %) 1) (partition-by identity (sort (concat (filter #(zero? (rem a %)) (range 1 (+ a 1))) (filter #(zero? (rem b %)) (range 1 (+ b 1))))))))))

(deftest test-gcm
  (is (= (gcm 10 5) 5)))

;Set Intersection

(defn my-intersection
  "Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common"
  [a b]
  (set (map #(first %)(filter #(> (count %) 1) (partition-by identity (sort (concat (into [] a) (into [] b))))))))

(deftest test-my-intersection
  (is (= (my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})))

;Simple Closures
(defn exp
  "Given a positive integer n, return a function (f x) which computes xn."
  [n]
  (fn [x]
    (reduce * (repeat n x))))

(deftest test-exp
  (is (= 256 ((exp 2) 16))))

;Re-implement Iterate
(defn my-iterate
  "Given a side-effect free function f and an initial value x write a function which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc."
  [f x]
    (lazy-seq (cons x (my-iterate f (f x)))))

(deftest test-my-iterate
  (is (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])))

;Comparison
(defn comparison
  "Return a keyword describing the relationship between the two items.
  x = y → :eq; x > y → :gt; x < y → :lt"
  [cmp a b]
  (cond
    (= ((comparator cmp) a b) 0) :eq
    (= ((comparator cmp) a b) 1) :gt
    (= ((comparator cmp) a b) -1) :lt))


(deftest test-comparison
  (is (= :gt (comparison < 5 1)))
  (is (= :eq (comparison (fn [x y] (< (count x) (count y))) "pear" "plum")))
  (is (= :lt (comparison (fn [x y] (< (mod x 5) (mod y 5))) 21 3))))

;Cartesian Product
(defn prod
  "Write a function which calculates the Cartesian product of two sets."
  [a b]
  (set (for [i a j b] [i j])))

(deftest test-prod
  (is (= (prod #{1 2 3} #{4 5}) #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})))

;Product Digits
(defn product-digits
  "Write a function which multiplies two numbers and returns the result as a sequence of its digits."
  [a b]
  (loop [res (* a b)
         digits ()]
    (if (zero? res)
      digits
      (recur (int (/ res 10)) (cons (rem res 10) digits )))))

(deftest test-product-digits
  (is (= (product-digits 99 9) [8 9 1])))

;Group A Sequence
(defn group
  "Given a function f and a sequence s, write a function which returns a map.
  Keys: f applied to each item in s. Values: vector of corresponding items in the order they appear in s."
  [f s]
  (into {} (map #(vector (f (first %)) %) (partition-by f (sort s)))))

(deftest test-group
  (is (= (group #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
  (is (= (group #(apply / %) [[1 2] [2 4] [4 6] [3 6]]) {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})))

;Symmetric Difference
(defn diff
  "Returns the set of items belonging to one but not both of the two sets."
  [a b]
  ;(union (difference a b) (difference b a)))
  (set (map #(first %)(filter #(= (count %) 1) (partition-by identity (sort (concat (into [] a) (into [] b))))))))

(deftest test-diff
  (is (= (diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})))

;dot product
(defn dot
  "dot product of two sequences (both vectors will have the same length."
  [a b]
  (reduce + (map * a b)))


(deftest test-dot
  (is (= 0 (dot [0 1 0] [1 0 0]))))

;Read a binary
(defn read-binary
  "Convert a binary number, provided in the form of a string, to its numerical value."
  [bin]
  (reduce (fn [accum digit] (+ digit (* 2 accum))) 0 (map #(if (= % '("1")) 1 0) (partition 1 (re-seq #".{1}" bin)))))

(deftest test-read-binary
  (is (= 0  (read-binary "0")))
  (is (= 7  (read-binary "111"))))

;Infix Calculator
(defn infix
  "Write a function that accepts a variable length mathematical expression consisting of numbers and the operations +, -, *, and /.
  Calculate left to right."
  [& args]
  (reduce (fn [base [sig b]] (sig base b)) (first args) (partition 2 (rest args))))

(deftest test-infix
  (is (= 4 (infix 2 + 5 - 3))))

;Indexing Sequences
(defn indexing
  "Transform a sequence into a sequence of pairs containing the original elements along with their index."
  [seq]
  (map-indexed (fn [index item] [item index]) seq))

(deftest test-indexing
  (is (= (indexing [:a :b :c]) [[:a 0] [:b 1] [:c 2]])))

;Pascal Triangle
(defn pascal
  "Write a function which returns the nth row of Pascal's Triangle. "
  [n]
  (if (= n 1)
    '(1)
    (map #(apply + %)
       (partition 2 1
                  (lazy-cat '(0) (pascal (dec n)) '(0))))))

(deftest test-pascal
  (is (= (pascal 11) [1 10 45 120 210 252 210 120 45 10 1])))

;Reimplement Map
(defn my-map
  "Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s."
  [f seq]
  ;(if (not-empty seq)
   ; (lazy-seq (cons (f (first seq)) (my-map f (next seq))))))
  (if (> (count seq) 0)
    (lazy-seq (cons (f (first seq)) (my-map f (next seq)))))
  )

  ;(lazy-seq (for [ i seq] (f i)))

(deftest test-my-map
  (is (= [3 4 5 6 7] (my-map inc [2 3 4 5 6]))))

;Sum of square of digitis
(defn square
  "Return the count of how many elements are smaller than the sum of their squared component digits.
  For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared."
  [col]

  (count (remove nil? (map #( if (> (loop [res % digits '(0)]
                                      (if (zero? res) (reduce + digits)
                                        (recur (int (/ res 10)) (cons (* (rem res 10) (rem res 10)) digits)))) %) %) col))))

(deftest test-square
   (is (= 8 (square (range 10)))))

;Recognize playing cards
(defn recognize
  "Write a function which converts the string into a map of {:suit s, :rank r}."
  [card]
  (let [suits {\S :spade, \H :heart, \D :diamond, \C :club}
        ranks {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]
    (zipmap [:suit :rank] (vector (suits (first card)) (ranks (second card))))))

(deftest test-recognize
  (is (= {:suit :diamond :rank 10} (recognize "DQ")))
  (is (= {:suit :heart :rank 3} (recognize "H5")))
  (is (= {:suit :club :rank 12} (recognize "CA"))))


;Intro to Destructuring
(= 3
   (let [[operation args] [+ (range 3)]] (apply operation args)) ; 1 + 2 = 3
   (let [[[operation args] b] [[+ 1] 2]] (operation args b)) ;1 + 2 = 3
   (let [[operation args] [inc 2]] (operation args))) ;inc 2 = 3

;Least Common Multiple
;Eucledian -> ( a * b) / (gcd)
(defn lcm
  "Write a function which calculates the least common multiple. Your function should accept a variable number of positive integers or ratios."
  [& args]
    (let [gcd (fn gcd [a b] (if (= 0 b) a (recur b (rem a b))))]
      (reduce (fn lcm [%1 %2] (/ (* %1 %2) (gcd %1 %2))) args)))

(deftest test-lcm
  (is (= (lcm 2 3) 6))
  (is (= (lcm 5 3 7) 105))
  (is (= (lcm 3/4 1/6) 3/2)))

;Beauty is Symmetry
(defn symmetry
  "Determine whether or not a given binary tree is symmetric. (Left side Mirror of right side)"
  [tree]
  (let [right (second tree) left (last tree)
        walk (fn walk [left right]
               (let [rol (first left) ll (second left) rl (last left)
                     ror (first right) lr (second right) rr (last right)]
                 (cond
                   (and (sequential? left) (sequential? right)) ;ambos son arreglos
                    (and (= rol ror) (walk ll rr) (walk lr rl)) ;compara a los padres. y hace llamadas recursivas en espejo
                   :else (= right left) ;compara hojas
                   )))]
    (walk right left)))

(deftest test-symmetry
  (is (= (symmetry '(:a (:b nil nil) (:b nil nil))) true))
  (is (= (symmetry '(:a (:b nil nil) nil)) false))
  (is (= (symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                      [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]]) true))
  (is (= (symmetry [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                       [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]]) false)))

;To Tree or no To Tree
(defn tree?
  "Checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child."
   [tree]
   (or (nil? tree) (and (sequential? tree) (= 3 (count tree)) (every? tree? (rest tree)))))

(deftest test-tree?
  (is (= (tree? '(:a (:b nil nil) nil)) true))
  (is (= (tree? '(:a (:b nil nil))) false))
  (is (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]]) true))
  (is (= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false))
  (is (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil]) false))
  (is (= (tree? '(:a nil ())) false)))

;Pazcal Trapezoide
(defn trap
  "where each next one is constructed from the previous following the rules used in Pascal's Triangle"
  [v]
 (iterate #(map +' (cons 0 %) (conj % 0)) v)) ; 0 n n n + n n n 0

(deftest test-trap
  (is (= (take 2 (trap [3 1 2])) [[3 1 2] [3 4 3 2]])))

(run-tests)
