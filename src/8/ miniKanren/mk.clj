(require ' [clojure.core.logic :as l])

;Operation prefixed with the name of the module l/
;/== -> unifies

;Conjunction -> ;Both NEED to be true, else it'll fail
(l/run 1 [p q] (l/== q 5) (l/== p q))

;Conde -> or, n possible value of q (* all possible result, may be endless loop)
;             n = 2 therefore two results are available
(l/run 2 [q] (l/conde [(l/== q 5)] [(l/== q 6)]))

(l/run* [p q] (l/conde [(l/== q 5) (l/== p 7)]
                       [(l/== q 5)] ;_0 anything
                       [(l/== p 5)]))

;Use for test (expr) (/l== q :Yes)
(l/run 1 [q] (l/conso 4 [1 2 3] [4 1 2 3])) ;_0, anything
;lasto lst x -> returns the last of the list

;declare  the Pattern Matching
(l/defne lasto
       "Logical function that succeeds if the last element of lst is x."
       [lst x]
       ([[x] x])
       ([[head . tail] x]
         (lasto tail x)) ; left . right -> first . rest -> lst = head, x = tail
         )

;Test -> (l/run 1 [q] (lasto [1 2 3] 3)(l/== q :Yes))
          ;=> (:Yes)

        ;(l/run 1 [q] (lasto [1 2 3] q))
        ;=> (3)

;(dupo [2 5 7 8] [2 2 5 5 7 7 8 8]
(l/defne dupo
         [lst result]
         ([[] []])
         ([[head . tail] [head head . temp]] ;match vector to lst vector -> [[1 2] [1 1 temp]] -> (dupo [2] [2 2])
           (dupo tail temp))
         )

;concat = appendo -> (l/run 1 [q] (l/appendo [1 2 3] [4 5 6] q))
(l/defne reverseo
         [lst result]
         ([[] []])
         ([[head . tail] result]
           (l/fresh [temp]
                    (reverseo tail temp) ;order of reverseo and appendo doesn't matter
                    (l/appendo temp [head] result)) ;create new variable
           ;[[1 2 3] result] ->  head -> 1; tail -> [2 3]; temp -> [3 2]
           ))

(l/defne twino
         [lst]
         ([[x x]]))
;(l/run 1 [q] (twino [:a :a]))
;(l/run 1 [q] (twino [1 1 1]))

(l/defne anti-twino
         [lst]
         ([[x y]]
           (l/!= x y))) ;don't unify

;enlisto [1 2 3] = [[1][2][3]]
;l/run* [q] (enlisto [1 1 1 1] q))
(l/defne enlisto
         [lst result]
         ([[][]])
         ([[head . tail] [[head] . temp]]
           (enlisto  tail temp))) ;[1 2 3] [[1] . temp]; head = 1; tail = [2 3]; (enlisto [2 3] temp) -> temp = [[2] [3]]

;(fn x (fn y)) -> (fn x t)(fn y t)
