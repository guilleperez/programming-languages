(use 'clojure.test)
(use 'clojure.math.numeric-tower)

; Tip: ºC = (ºF - 32) × 5 ÷ 9

(defn f2c [f]
  "Write a function called f2c that takes x degrees Fahrenheit and converts them to degrees Celsius."
  ( /(* (- f 32) 5) 9)
  )


(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0)))
  (is (= 0.0 (f2c 0.0))))

(if (< 1 4) (+ 2 3) (* 4 5))


(defn sign
  "Takes an integer value n. It returns -1 if n is negative, 1 if n is positive greater than zero, or 0 if n is zero."
  [n]
  (if (< n 0)
    -1
    (if (>= n 1)
      1
      0
    )
  )
  )

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(defn roots
  "Returns a vector containing the two possible roots that solve a quadratic equation given its three coefficients (a, b, c)."
  [a,b,c]
   ;let [t1 (-b)
   ;     t2 (sqrt (- (* b b) (* (* 4 a) c) )))
   ;     t3 (* 2 a) ]
   ;    [(/ (+ t1 t2) t3)
   ;     (/ (- t1 t2) t3)]
   ; ))
  [
   (/ (+ (- b) (sqrt (- (* b b) (* (* 4 a) c) ))) (* 2 a))
   (/ (- (- b) (sqrt (- (* b b) (* (* 4 a) c)  ))) (* 2 a))
   ]
  )

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(run-tests)



(defn bmi
  "Computes the body mass index of a person"
  [weight height]
  (let [BMI (/ weight (expt height 2))]
    ;(if (< BMI 20) 'underwight)
    ;(if (< BMI 25) 'normal)
    ;(if  (< BMI 30) 'obese2)
    ;(if (< BMI 40) 'obese2 'obese3)
    (cond
      (< BMI 20) 'underwight
      (< BMI 25) 'normal
      (< BMI 30) 'obese1
      (< BMI 40) 'obese2
      :else 'obese3)))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(defn fact
  "Get the factoril of n"
  [n]
  (if (= n 0)
    1
    (* n (fact(dec n)))))

(deftest test-factorial
  (is (= 1 (fact 0)))
  (is (= 1 (fact 1)))
  (is (= 2 (fact 2)))
  (is (= 6 (fact 3))))

(defn fact-loop
  "Computes the factorial on n using loop/recur."
  [n]
  (loop [i 1
         result 1]
    (if (> i n)
      result
      (recur (inc i) (* result i)))))

(deftest test-factorial-loop
  (is (= 1 (fact-loop 0)))
  (is (= 1 (fact-loop 1)))
  (is (= 6 (fact-loop 3)))
  (is (= 120 (fact-loop 5))))

; 0 - ()
; 1 - (())
; 1 - (()())
(defn is-zero[n] (= n ()))
(defn add-one[n] (cons () n))
(defn sub-one[n] (rest n))

(defn add [a b]
  (if (is-zero a)
    b
    (add-one (add (sub-one a) b))))

(defn sub [a b]
  (if (is-zero b)
    a
    (sub-one (sub a (sub-one b)))))

(def z ())
(defn mul [a b]
  (if (is-zero a)
    z
    (add b (mul (sub-one a) b))))

(defn is-less
  [a b]
  (if (is-zero a)
    (if (is-zero b)
      false
      true)
    (if (is-zero b)
      false))
  (is-less (sub-one a) (sub-one b)))

(defn div
  "Computes the division of two numbers"
  [a b]
  (if (is-less a b)
    z
    (add-one (div (sub a b) b))))

(defn delete
  "Remove all occurrences of x in lst, which might contain nested lists"
  [x lst]
  (cond
    (empty? lst)
    ()

    (list? (first lst))
    (cond (delete x (first lst))
          (delete x (rest lst))

          (= x (first lst))
          (delete x (rest lst))

          :else
          (cons (first lst)
                (delete x (rest lst))))))