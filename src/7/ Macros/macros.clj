;(eval (expr)

;(my-macro (+ 1 2) -> macro receives the raw data structure
;raw data structure ) + -> 1 -> 2

;identity function
;arguments are sent evaluated -> (my-function (+ 3 4)) -> 7
(defn my-function
  [x]
  x)

;identity macro
;macroexpand-1 '(my-function (+ 3 4)) -> (+ 1 2)
(defmacro my-macro
  [x]
  x)

;LET (var expr) body => ((fn [var]  body) expr)
;(LET (hello (+ 1 2)) (* hello hello))
(defmacro LET
  [[var expr] body]

  (list
    (list
      'fn [var]  body) expr))
;(list (+ 1 2) (comment (* 3 4)) (/
;
;
;
; 0 5)) -> (3 nil 2)
(defmacro COMMENT
  [& x]
  nil)

;(+ 2 (debug (* 3 5 ))) -> Debug (* 3 5) : 15
;(defmacro debug
;  [expr]
;  (list
;    'let
;    ['value expr]
;    (list 'print "Debug ")
;    (list 'print (list 'quote expr))
;    (list 'print ":")
;    (list 'println 'value)
;    'value))

(defmacro debug
  [expr]
  `(let [value# ~expr] ;Produces a new name value
     (println "Debug" ~(list 'quote expr) ":" value#)
     value#)
  )

(defmacro AND
  ([] true)
  ([x] x)
  ([a b & expr]
   `(let [temp# ~a]
      (if temp#
        (AND ~b ~@expr)
        temp#))))

;exprs -> (exp1 exp2
;           :THEN  exp3 exp4 exp5
;           :ELSE exp6 exp7 exp8)

(defn find-between
  [start end exprs]
  (->> exprs
       (drop-while #(not= start %)) ;find else
       rest
       (take-while #(not= end %)))) ;until end


(defmacro IF
  [condition & exprs]
  `(if ~condition
     (do ~@(find-between :THEN :ELSE exprs)) ;@ -> unquote list of (find-between, get only its values)
     (do ~@(find-between :ELSE :THEN exprs))))

;(IF (< 3 1)
;    :ELSE (println "Else") 'else
;    :THEN (println "Then") 'then)

;(macroexpand-1 '(debug (+ 1 2)) )
;=> (let [value (+ 1 2)] (print "Debug ") (print (quote (+ 1 2))) (print ":") (println value) value)


