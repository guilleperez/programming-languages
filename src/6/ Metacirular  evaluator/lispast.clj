;==========================================================
; This file contains the complete Lisp metacircular
; evaluator program written in class (plus some unit
; tests). Modify this code in order to add to the
; interpreter a new special form called 'dotimes'
; with the following syntax:
;
;     (dotimes (var count) body)
;
; where 'var' is a symbol and 'count' and 'body' are
; expressions. This construct executes 'body' (which must
; perform some side effect operation, typically printing)
; once for each integer from 0 (inclusive) to 'count'
; (exclusive), binding the variable 'var' to the integer
; for the current iteration. This special form always
; returns nil.
;
; Example 1:
;
;     ($eval ’(dotimes (i (+ 2 2))
;               (println "Line" i))
;            {’println println, ’+ +})
;     Line 0
;     Line 1
;     Line 2
;     Line 3
;     => nil
;
; Example 2:
;
;    ($eval ’(dotimes (x 10)
;              (pr x))
;           {’pr pr})
;    0123456789
;    => nil
;
;==========================================================

(use 'clojure.test)
(import 'clojure.lang.IFn)
(declare evaluate)

;==========================================================
(deftype Closure
  [env params body]
  IFn
  (invoke
    [self args]
    (evaluate body
              (merge @env (zipmap params args))))
  (applyTo
    [self args]
    (self args)))

;==========================================================
(defn third
  "Returns third element of lst."
  [lst]
  (nth lst 2))

;==========================================================
(defn fourth
  "Returns fourth element of lst."
  [lst]
  (nth lst 3))

;==========================================================
(defn evaluate
  "Evaluate an expression expr in the context of
  an environment env and return the result."
  [expr env]
  (cond

    (symbol? expr) ; expr is a variable reference
    (if (contains? env expr)
      (get env expr)
      (throw (RuntimeException.
               (str "Unbound variable: " expr))))

    (list? expr) ; expr is a list, so keep checking what's in it
    (case (first expr)

      nil ; expr is an empty list
      ()

      quote ; expr is a "quote" special form
      (second expr)

      if ; expr is an "if" special form
      (if (evaluate (second expr) env)
        (evaluate (third expr) env)
        (evaluate (fourth expr) env))

      lambda ; expr is a "lambda" special form
      (->Closure (atom env) (second expr) (third expr))

      label ; expr is a "label" special form
      (let [lambda-expr (evaluate (third expr) env)]
        (swap! (.env lambda-expr)
               #(assoc % (second expr) lambda-expr))
        lambda-expr)

      dotimes ; expr is a "dotimes" special form
      (doseq [value (range (evaluate (second (second expr)) env))]
        (evaluate (third expr)
                  (assoc env (first (second expr)) value)))

      ; else, expr is a function invocation
      (apply (evaluate (first expr) env)
             (map #(evaluate % env)
                  (rest expr))))

    ; expr is something else, so let it evaluate to itself
    :else
    expr))

;==========================================================
(deftest test-var-ref
  (is (= 15 (evaluate 'c
                      {'a 4, 'b 8, 'c 15})))
  (is (thrown? RuntimeException
               (evaluate 'x
                         {'a 4, 'b 8, 'c 15}))))

;==========================================================
(deftest test-itself
  (is (= 42 (evaluate 42 {})))
  (is (= true (evaluate true {})))
  (is (= false (evaluate false {})))
  (is (= nil (evaluate nil {})))
  (is (= "hello" (evaluate "hello" {}))))

;==========================================================
(deftest test-empty-list
  (is (= () (evaluate () {}))))

;==========================================================
(deftest test-quote
  (is (= 'a
         (evaluate '(quote a) {})))
  (is (= '(1 2 3)
         (evaluate '(quote (1 2 3)) {})))
  (is (= '42 (evaluate '(quote 42) {}))))

;==========================================================
(deftest test-if
  (is (= 1 (evaluate '(if true 1 2) {})))
  (is (= 2 (evaluate '(if false 1 2) {}))))

;==========================================================
(deftest test-function-invocation
  (is (= 3
         (evaluate '(f 1 2)
                   {'f +})))
  (is (= 'a
         (evaluate '(g (quote (a b c d e)))
                   {'g first})))
  (is (= '(a b c)
         (evaluate '(cons x y)
                   {'cons cons, 'x 'a, 'y '(b c)})))
  (is (= 55
         (evaluate '(+ 1 2 3 4 5 6 7 8 9 10)
                   {'+ +})))
  (is (= '(a b c)
         (evaluate '(apply cons (quote (a (b c))))
                   {'apply apply, 'cons cons}))))

;==========================================================
(deftest test-lambda
  (let [c (evaluate '(lambda (x)
                             (* x 2))
                    {'* *})]
    (is (instance? Closure c))
    (is (= @(.env c) {'* *}))
    (is (= (.params c) '(x)))
    (is (= (.body c) '(* x 2)))
    (is (= 42 (c '(21))))
    (is (= 42 (apply c '(21)))))
  (is (= 8
         (evaluate '((lambda (f x)
                             (f (f (f x))))
                      (lambda (x) (* x 2))
                      1)
                   {'* *}))))

;==========================================================
(deftest test-label
  (is (= '(a a b b c c)
         (evaluate
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
         (evaluate
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
         (with-out-str (evaluate '(dotimes (x 0)
                                    (println x))
                                 {'println println}))))
  (is (= "0123456789"
         (with-out-str (evaluate '(dotimes (x 10)
                                    (pr x))
                                 {'pr pr}))))
  (is (= "Line 0\nLine 1\nLine 2\nLine 3\n"
         (with-out-str (evaluate '(dotimes (i (+ 2 2))
                                    (println "Line" i))
                                 {'println println, '+ +}))))
  (is (= "1-4-9-16-25-36-49-64-81-100-"
         (with-out-str (evaluate '(dotimes (some-var (* 2 5))
                                    (printf "%d-"
                                            ((lambda (x) (* x x))
                                              (inc some-var))))
                                 {'printf printf, '* *, 'inc inc}))))
  (is (= "****************************************************************************************************"
         (with-out-str (evaluate '(dotimes (mxyzptlk (* 2 2 5 5))
                                    (print "*"))
                                 {'print print, '* *})))))

;==========================================================
(run-tests)