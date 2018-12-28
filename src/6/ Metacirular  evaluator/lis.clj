;Metacirucular evaluator

(use 'clojure.test)
(import 'clojure.lang.IFn) ;Java interface for a function
(declare $eval)

(deftype Closure
  [env params body]
  IFn ;Extends IFn -> Java
  (invoke
    [self args]
    ($eval body (merge @env (zipmap params args)))) ;($eval body (merge globals (lambda (x) (* x 2))) -> ($eval 10 globals {...  (lambda (x) (* x 2))})
    ;env {'x x} -> {'+ +, 'x 3, 'y 4}
    ; params (x y)
    ; args (3 4)
    ;(zipmap '(x y) '(3 4)) -> {x 3, y 4}

  (applyTo
    [self args]
    (self args)))

;Helper functions
(defn third
  [lst]
  (nth lst 2))

(defn fourth
  [lst]
  (nth lst 3))

;$eval expression environment
(defn $eval
  [expr env]
  (cond

    ;1. Variable references -> ($eval '(x) {x 1})
    (symbol? expr)
      (if (contains? env expr)
        (get env expr)
        (throw (RuntimeException. (str "Unbound variable: " expr))))


    ;2. Special forms
    (list? expr) ;($eval (a b) {})
      (case (first expr) ;verifica el valor de (first expre)

        nil () ;($eval nil {}) expr is an empty list returns ()

        quote (second expr) ;($eval '(quote x) {}) expr is a '. returns symbol -> x

        if ;($eval '(if (x) x)
          (if ($eval (second expr) env)
            ($eval (third expr) env)
            ($eval (fourth expr) env))

        lambda ;($eval '(lambda (x) x) -> closure (env, params, body)
          (->Closure (atom env) (second expr) (third expr))

        label ;expr -> (label name (lambda )) recursive functions
        (let [closure($eval (third expr) env)] ;A closure is a function that has access to some named value/variable outside its own scope, so from a higher scope surrounding the function when it was created
          (swap! (.env closure) ;reassign the value of the env
                 #(assoc % (second expr) closure)) ; {x 5, y 6} -> add name function -> (assoc map env value)
                 closure) ;return the new closure

        dotimes
        (let [var (first (second expr))
              limit ($eval (second (second expr)) env)]
              ;doseq = for with side effects
             (doseq [i (range limit)] ($eval (third expr) (assoc env var i))) ;assoc solves error: Unbound variable: i -> (assoc '[a b c] key value)
          )
        ;(doseq [i (range ($eval (second (second expr)) env))] ($eval (third expr) (assoc env (first (second expr)) i)) )
        let
        ($eval (third expr)
               (assoc env (first (second expr)) ($eval (second (second expr)) env)))

        ;default clause: function invocation -> ($eval '(f 1) {}) -> apply f [arg1 arg2 ...]
        ;($eval '(+ 1 2 )  {'+ +}) => 3
        (apply ($eval (first expr) env)
               (map #($eval % env) (rest expr))))

    ;3. Everyting else evaluates to itself
    :else expr))

;4. Atom -> empty list or anything that is not a list
;5. eq =
;6. car first
;7. cdr rest
;8. cons cons
;9. cond if
(def globals {'EQ =
              'CAR first
              'CDR rest
              'CONS cons
              'ATOM #(or (= % ()) (not (list? %)))
              '* *
              '+ +})

;($eval '((lambda (f x) (f (f x)))
;       (lambda (x) (* x 2))
;       10)
;  globals)

;($eval '((label DUP (lambda (x)
;                            if (EQ lst ())
;                            ()
;                            (CONS (first lst) (CONS (CAR lst)
;                                                    (DUP (CDR lst)))))
;                (quote (A B C))))
;       globals)


;((label name (lambda ...)) 5)
;             (lambda ...) = closure
;        name = env -> convert env to atom and modify
;atom -> @atom
;reassign a value -> swap! a inc

;Obtain the values of closure
;.env closure
;.params closure
;.name closure

;==========================================================
(deftest test-var-ref
  (is (= 15 ($eval 'c
                   {'a 4, 'b 8, 'c 15})))
  (is (thrown? RuntimeException
               ($eval 'x
                      {'a 4, 'b 8, 'c 15}))))

;==========================================================
(deftest test-itself
  (is (= 42 ($eval 42 {})))
  (is (= true ($eval true {})))
  (is (= false ($eval false {})))
  (is (= nil ($eval nil {})))
  (is (= "hello" ($eval "hello" {}))))

;==========================================================
(deftest test-empty-list
  (is (= () ($eval () {}))))

;==========================================================
(deftest test-quote
  (is (= 'a
         ($eval '(quote a) {})))
  (is (= '(1 2 3)
         ($eval '(quote (1 2 3)) {})))
  (is (= '42 ($eval '(quote 42) {}))))

;==========================================================
(deftest test-if
  (is (= 1 ($eval '(if true 1 2) {})))
  (is (= 2 ($eval '(if false 1 2) {}))))

;==========================================================
(deftest test-function-invocation
  (is (= 3
         ($eval '(f 1 2)
                {'f +})))
  (is (= 'a
         ($eval '(g (quote (a b c d e)))
                {'g first})))
  (is (= '(a b c)
         ($eval '(cons x y)
                {'cons cons, 'x 'a, 'y '(b c)})))
  (is (= 55
         ($eval '(+ 1 2 3 4 5 6 7 8 9 10)
                {'+ +})))
  (is (= '(a b c)
         ($eval '(apply cons (quote (a (b c))))
                {'apply apply, 'cons cons}))))

;==========================================================
(deftest test-lambda
  (let [c ($eval '(lambda (x)
                          (* x 2))
                 {'* *})]
    (is (instance? Closure c))
    (is (= @(.env c) {'* *}))
    (is (= (.params c) '(x)))
    (is (= (.body c) '(* x 2)))
    (is (= 42 (c '(21))))
    (is (= 42 (apply c '(21)))))
  (is (= 8
         ($eval '((lambda (f x)
                          (f (f (f x))))
                   (lambda (x) (* x 2))
                   1)
                {'* *}))))

;==========================================================
(deftest test-label
  (is (= '(a a b b c c)
         ($eval
           '((label dup (lambda (lst)
                                (if (eq lst ())
                                  ()
                                  (cons (car lst)
                                        (cons (car lst)
                                              (dup (cdr lst)))))))
              (quote (a b c)))
           {'eq   =
            'cons cons
            'car  first
            'cdr  rest})))
  (is (= '(1 4 9 16)
         ($eval
           '((label mapcar (lambda (fun lst)
                                   (if (eq lst ())
                                     ()
                                     (cons (fun (car lst))
                                           (mapcar fun (cdr lst))))))
              (lambda (x) (* x x))
              (quote (1 2 3 4)))
           {'eq   =
            'cons cons
            'car  first
            'cdr  rest
            '*    *}))))

;==========================================================
(deftest test-dotimes
  (is (= ""
         (with-out-str ($eval '(dotimes (x 0)
                                 (println x))
                              {'println println}))))
  (is (= "0123456789"
         (with-out-str ($eval '(dotimes (x 10)
                                 (pr x))
                              {'pr pr}))))
  (is (= "Line 0\nLine 1\nLine 2\nLine 3\n"
         (with-out-str ($eval '(dotimes (i (+ 2 2))
                                 (println "Line" i))
                              {'println println, '+ +}))))
  (is (= "1-4-9-16-25-36-49-64-81-100-"
         (with-out-str ($eval '(dotimes (some-var (* 2 5))
                                 (printf "%d-"
                                         ((lambda (x) (* x x))
                                           (inc some-var))))
                              {'printf printf, '* *, 'inc inc}))))
  (is (= "****************************************************************************************************"
         (with-out-str ($eval '(dotimes (mxyzptlk (* 2 2 5 5))
                                 (print "*"))
                              {'print print, '* *})))))
;==========================================================
(deftest test-let
  (is (= 7
         ($eval '(let (a 7) a) {})))
  (is (= 42
         ($eval '(let (x 6)
                   (* 7 x))
                {'* *})))
  (is (= 111
         ($eval '(let (x (* 2 5))
                   (let (y (+ 1 x))
                     (+ 1 (* y x))))
                {'+ +
                 '* *})))
  (is (= 60
         ($eval '((lambda (x y)
                          (let (y (+ 1 y))
                            (* x y)))
                   10
                   5)
                {'* *
                 '+ +})))
  (is (= '(a b c d)
         ($eval '(let (one (quote (c d)))
                   (let (two (cons (quote b) one))
                     (let (three (cons (quote a) two))
                       three)))
                {'cons cons})))
  (is (= 8
         ($eval '(((lambda (x y)
                           (let (z (let (r) (* x y)) r)
                             (lambda (w)
                                     (let (t (+ x (let (a y) a) z w))
                                       (let (t t) t))))) 1 2) 3)
                {'* *
                 '+ +}))))

(run-tests)

