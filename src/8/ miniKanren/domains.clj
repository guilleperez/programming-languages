(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

;(l/run 1 [q] (fd/+ 2 3 q)) => (5)

(l/defne addo
         [lst result]
         ([[] 0])
         ([[head . tail] result]
           (l/fresh [temp]
                    (addo tail temp)
                    (fd/+ head temp result))))

(defn magic-square
        []
        (l/run*
          [q1 q2 q3
           q4 q5 q6
           q7 q8 q9]
          (fd/in q1 q2 q3 q4 q5 q6 q7 q8 q9 (fd/interval 1 9))
          (l/distincto [q1 q2 q3 q4 q5 q6 q7 q8 q9])
          (addo [q1 q2 q3] 15)
          (addo [q4 q5 q6] 15)
          (addo [q7 q8 q9] 15)
          (addo [q1 q4 q7] 15)
          (addo [q2 q5 q8] 15)
          (addo [q3 q6 q9] 15)
          (addo [q1 q5 q9] 15)
          (addo [q3 q5 q7] 15)))
;(l/run 1 [p q] (fd/in p q (fd/interval 0 10)) (fd/+ p q 5))
;=> ([0 5])
;(l/run 1 [q] (fd/in q (fd/interval 0 10)) (addo [1 q q] 5))
;=> (2)

(l/defne largesto
         [lst result]
         ([[x] x])
         ([[head . tail] head]
         (l/fresh [temp]
                  (largesto tail temp)
                  (fd/> head temp)))
         ([[head . tail] temp]
           (largesto tail temp)
           (fd/>= temp head)))