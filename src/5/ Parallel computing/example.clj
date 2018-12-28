(use 'clojure.test)

;pmap -> parallel map -> concurrency
;use it in small sequences, big sequences create to many
; threads and it will slow down the system
;(time (doall (pmap fibo '(42 42 42 42))))) -> get time, doall avoids lazy sequence

(pmap fibo '(42 42 42 42))
(defn fib
  "Return fibonacci "
  [n]
  (if (< n 2)
    n
    (+' (fib (- n 1)) (fib (- n 2)))))

;Paralellism uses Java parallelism
;create a Thread
;(. start
;   (Thread . (fn [])))

(run-tests)
