;----------------------------------------------------------
; Problem Set: MiniKanren
; Date: November 25, 2018.
; Authors:
;          A01377515 Pablo Alejandro Sánchez Tadeo
;          A01377162 Guillermo Perez Trueba
;----------------------------------------------------------

(use 'clojure.test)
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;2. (palindromeo lst): This logic function succeeds if lst is a palindrome list (it reads the same from left to right than from right to left).

(l/defne  reverseo
  [lst result]
  ([[] []])
  ([[head . tail] result]
    (l/fresh [temp]
             (reverseo tail temp)
             (l/appendo temp [head] result))))


(l/defne palindromeo
         [lst]
         ([result]
           (l/fresh [temp]
                  (reverseo lst result))))

(deftest test-palindromeo
  (is (= [:yes]
         (l/run 1 [q] (palindromeo []) (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (palindromeo [:a])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (palindromeo [:a :b :c :b :a])
                (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
                (palindromeo [:a :b :c :d])
                (l/== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (l/run 7 [q]
                (palindromeo q)))))

;4 (evensizeo lst) and (oddsizeo lst): These two logic functions should be defined in a mutually recursive fashion.
; That is, each one should be defined in terms of the other one. These functions succeed if the number of elements in lst is even or odd, respectively.
; Tip: Remember to use the 'declare' macro to indicate that a certain function can be called before it’s actually defined.
; This is necessary whenever you define mutually recursive functions.

(declare evensizeo)

(l/defne oddsizeo ; [2 3]
         [lst]
         ([result]
           (l/fresh [head tail]  ;[3 ()]
                    (l/conso head tail lst)
                    (l/== () tail)))
         ([result]
           (l/fresh [head tail]  ;[2 ()]
                    (l/conso head tail lst)
                    (l/!= () tail)
                    (evensizeo tail))))

(l/defne evensizeo ;[1 2 3]
         [lst]
         ([[]])
         ([result]
         (l/fresh [head tail]
                  (l/conso head tail lst) ; ([head (tail)]) sobre lst
                  (oddsizeo tail))) ; [2 3]
           )

(deftest test-evensizeo-oddsizeo
  (is (= [:yes]
         (l/run 1 [q]
                (evensizeo [])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (oddsizeo [:x])
                (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
                (evensizeo [:x])
                (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
                (oddsizeo [])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (evensizeo [:a :b :c :d :e :f])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (oddsizeo [:a :b :c :d :e])
                (l/== q :yes))))
  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (l/run 5 [q]
                (evensizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (l/run 5 [q]
                (oddsizeo q)))))



;6 (equalo lst): This logic function succeeds if all the elements contained in lst unify to the same value,
; otherwise fails. The function should always succeed if lst is empty or has only one element.

(l/defne equalo
         [lst]
         ([[]])
         ([[x]])
         ([result]
           (l/fresh [a b tail rest]
                    (l/conso a tail lst)
                    (l/conso b rest tail)
                    (l/== a b)
                    (equalo tail))))

(deftest test-equalo
  (is (= [:yes]
         (l/run 1 [q]
                (equalo [])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (equalo [:x])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (equalo [:x :x])
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (equalo [:x :x :x :x :x])
                (l/== q :yes))))
  (is (= [:x]
         (l/run 1 [q]
                (equalo [:x :x q :x]))))
  (is (= '[_0]
         (l/run 1 [q]
                (equalo [q q q q q q]))))
  (is (= '([_0 _0 _0 _0 _0])
         (l/run 1 [q1 q2 q3 q4 q5]
                (equalo [q1 q2 q3 q4 q5]))))
  (is (= []
         (l/run 1 [q]
                (equalo [:x :y])
                (l/== q :yes))))
  (is (= []
         (l/run 1 [q1 q2]
                (equalo [q1 q1 q2 q1 q1])
                (l/!= q1 q2))))
  (is (= '([]
            [_0]
            [_0 _0]
            [_0 _0 _0]
            [_0 _0 _0 _0]
            [_0 _0 _0 _0 _0]
            [_0 _0 _0 _0 _0 _0])
         (l/run 7 [q]
                (equalo q)))))

;8. (facto n result): This logic function succeeds if the factorial of n is equal to result.
(l/defne facto
          [n result]
          ([0 1])
         ([n result]
           (l/fresh [prev fac]
                    (fd/- n 1 prev)
                    (fd/* n fac result)
                    (fd/>= n 1)
                    (facto prev fac))))

(deftest test-facto
  (is (= [1]
         (l/run 1 [q]
                (facto 0 q))))
  (is (= [1]
         (l/run 1 [q]
                (facto 1 q))))
  (is (= [720]
         (l/run 1 [q]
                (facto 6 q))))
  (is (= [2432902008176640000]
         (l/run 1 [q]
                (facto 20 q))))
  (is (= [0 1]
         (l/run 2 [q]
                (facto q 1))))
  (is (= [5]
         (l/run 1 [q]
                (facto q 120))))
  (is (= [10]
         (l/run 1 [q]
                (facto q 3628800))))
  (is (= [:yes]
         (l/run 1 [q]
                (facto 4 24)
                (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
                (facto 15 1307674368000)
                (l/== q :yes))))
  (is (= [[0 1]
          [1 1]
          [2 2]
          [3 6]
          [4 24]
          [5 120]
          [6 720]
          [7 5040]
          [8 40320]
          [9 362880]]
         (l/run 10 [n r]
                (facto n r)))))

; 10. (rangeo start end result): This logic function unifies result with a sequence of incremental integers from start to end (inclusive).

(l/defne rangeo
         [start end result]
         ([start end result]
           (l/fresh [temp next]
                    (fd/+ start 1 next)
                    (l/appendo [start] temp result)
                    (fd/< start end)
                    (rangeo next end temp)))
         ([start end result]
           (fd/> start end)
           (l/appendo [] [] result))
         ([end end result]
           (l/appendo [start] [] result)))

(deftest test-rangeo
  (is (= [[3 4 5 6 7 8 9 10]]
         (l/run 1 [q]
                (rangeo 3 10 q))))
  (is (= [[7]]
         (l/run 1 [q]
                (rangeo 7 7 q))))
  (is (= [[]]
         (l/run 1 [q]
                (rangeo 10 1 q))))
  (is (= [6]
         (l/run 1 [q]
                (fd/in q (fd/interval 1 10))
                (rangeo 2 q [2 3 4 5 6]))))
  (is (= [[2 6]]
         (l/run 1 [q1 q2]
                (fd/in q1 q2 (fd/interval 1 10))
                (rangeo q1 q2 [2 3 4 5 6]))))
  (is (= #{[]
           [1] [1 2] [1 2 3] [1 2 3 4]
           [2] [2 3] [2 3 4]
           [3] [3 4]
           [4]}
         (set
           (l/run* [q]
                   (l/fresh [start end]
                            (fd/in start end (fd/interval 1 4))
                            (rangeo start end q)))))))





(run-tests)
