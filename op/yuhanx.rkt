#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

#| Question 1: Using *** to show the changes I made in Comments |#

#| Question 2

The Lazy evaluation will result in a different semantics in some special cases
Since we have static scoping and dynamic scoping, the lazy evaluation will not effect our static scoping which is our enviornment.
However, it does effect the dynamic scoping which is our heap.
Therefore, if our program contains some expression which will update the heap and those expression might not be evaluated since we are using lazy evaluation,
Our semantics will be different with homework5.

The idea of the lazy evaluation is that we store the Expr for the variable directly in the env, like (x (+ 1 1)), instead of storing (x 2) in env, we store (x (+ 1 1)) in env.
When we want to use this variable, say x, in the later expression, we evaluate the Expr which belongs to this variable, which is (+ 1 1).

1.
When CondExpr (and, or) if the first conditions false (and) / true (or) the rest of the condiitons will not be evaluated.
If the rest of the condiitons contain any expression that will make changes to heap, those changes will not be evaluated and it will effect the semantics of the program.

Example 1:
(define test1
  '((and (gt 1 2) (lt (var ((x (ref 10))) x) 2))
1
(deref 1)
))
if we do (eval test1 '() '((1 free))) in our original one for homework5, it will result '(10 ((1 10)))
However, if we use lazy evaluation, the (lt (var ((x (ref 10))) x) 2) part will not be evaluated since (gt 1 2) is false and it's enough for 'and'
So the result will be '((exception fma) ((1 free)))

Exanmple 2:
(define test2
  '((or (gt 2 1) (lt (var ((x (ref 10))) x) 2))
(deref 1)
1
))
if we do (eval test2 '() '((1 free))) in our original one for homework5, it will result '(10 ((1 10)))
However, if we use lazy evaluation, the (lt (var ((x (ref 10))) x) 2) part will not be evaluated since (gt 2 1) is true and it's enough for 'or'
So the result will be '((exception fma) ((1 free)))

2.
Since we have changed all evaluation for VarExpr to lazy evaluation, if there is a varassign which will make change to heap such as (x (ref 10))
but this varassign is not evaluated in the later expressions, then the change will never be evaluated and it will effect the semantics of the program.

Example 1:
(define test3
  '(var ((x (ref 10)) (y 1)) (+ y (deref 1))))
if we do (eval test3 '() '((1 free))) in our original one for homework5, it will result '(11 ((1 10)))
However, if we use lazy evaluation, the (x (ref 10) part will not be evaluated since x is not called in the later expression, so the result will be '((exception fma) ((1 free)))

Moreover, if we have ( (x (ref 1)) (y (ref 5)) ) as our varassignseq, ((1 free) (2 free)) and y evaluate before x, then the heap will be ((1 5) (2 1)) instead of ((1 2) (2 5))
And this will effect our semantics.

3.
Same with VarExpr, we will have different semantics for evaluating ApplyF.
Since we use VarExpr to evaluate ApplyF, those args might not be evaluated if they are not called in the later expression. Then the result will be different.

Example 1:
(define test4
  '(fun ((f (x y z)) ((gt z y) x (deref 1)))
(apply (f ((ref 2) (+ 4 4) (+ 0 1))))))
if we do (eval test4 '() '((1 free))) in our original one for homework5, it will result '(2 ((1 2)))
However, if we use lazy evaluation, the (ref 2) part will not be evaluated since x is not called in the later expression, so the result will be '((exception fma) ((1 free)))

|#

(define test4
  '(fun ((f (x y z)) ((gt z y) x (deref 1)))
(apply (f ((ref 2) (+ 4 4) (+ 0 1))))))


;; to make sure I remember what I am using
(define (value result) (car result))
(define (heap result)  (cadr result))

;; returns '(value/exception heap)
(define (eval expr env heap)
  (cond
    ;; number
    [ (number? expr)  (list expr heap) ] 

    ;; variable
    [ (symbol? expr)  (findvalue expr env heap) ]

    ;; arithexpression: use evalarith
    [ (arithop (car expr))
      ;; then
      (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]

    ;; varexpression
    [ (equal? (car expr) 'var)
      ;; then
      (evalvarassign (cadr expr) (cadr (cdr expr)) env heap) ]

    ;; fun expression
    [ (equal? (car expr) 'fun)
      ;; then just add the function definition to the environment
      (eval (cadr (cdr expr)) (cons (cadr expr) env) heap) ]

    ;; application expression: use translate
    [ (equal? (car expr) 'apply)
      ;; then translate to varexpressions
      (eval (translate (cadr expr) env) env heap) ]

    ;; deref: use derefhandler - we will just pass the values there as
    ;; expressions inside deref cannot produce any exceptions 
    [ (equal? (car expr) 'deref)
      ;; then read from the heap              *** here we are using the new heap after evaluating the location
      (derefhandler (value (eval (cadr expr) env heap)) (cadr (eval (cadr expr) env heap)) ) ]

    ;; wref: use wrefhandler
    [ (equal? (car expr) 'wref)
      ;; then                                 *** here we are using the new heap after evaluating the location
      (wrefhandler (value (eval (cadr expr) env heap)) (value (eval (cadr (cdr expr)) env heap)) (cadr (eval (cadr expr) env heap))) ]

    ;; free: use freehandler
    [ (equal? (car expr) 'free)
      ;; then                                 *** here we are using the new heap after evaluating the location
      (freehandler (value (eval (cadr expr) env heap)) (cadr (eval (cadr expr) env heap))) ]

    ;; ref: use refhandler
    [ (equal? (car expr) 'ref)
      ;; then                                  *** here we are using the new heap after evaluating the value
      (refhandler (value (eval (cadr expr) env heap)) (cadr (eval (cadr expr) env heap))) ]

    ;; nothing else left: must be a cond expression
    [ else  (ifthenelse (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]
    ))

;; helpers
;; input: variable, environment
;; output: value to which the variable is mapped to in the environment
;;         It can be '(Cannot Evaluate) 
(define (findvalue x env heap)
  (if (null? env)  
      (list '(Cannot Evaluate) heap)
      (if (equal? (car (car env)) x)
          (eval (cadr (car env)) env heap) ;;*** instead of get the value directly, we evaluate it since we store an expression here.
          (findvalue x (cdr env) heap))))

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; evalarith
;; input: op = +, -, ...; others self explanatory
;; output: (value/exception heap)
(define (evalarith op expr1 expr2 env heap)
  (evalarith1 op (eval expr1 env heap) expr2 env))  ;; evaluate in one function and use in another
                                              ;; to avoid unnecessary recomputation

(define (evalarith1 op semexpr1 expr2 env)
  (if (list? (value semexpr1)) ;; expr1 evaluation produced an exception
      semexpr1
      (evalarith2 op (value semexpr1) (eval expr2 env (heap semexpr1)))))

(define (evalarith2 op valexpr1 semexpr2)
  (if (list? (value semexpr2)) ;; expr2 evaluation produced an exception
      semexpr2
      ;; else use myapply as before and pair it with the heap
      (list (myapply op valexpr1 (value semexpr2)) (heap semexpr2))))

;; evalvarassign
;; input: list of (variable expression), expr to evaluate, environment, heap
;; output: evaluate expr in new environment and heap or exception in between
(define (evalvarassign varassigns expr env heap)
  (if (null? varassigns)
      (eval expr env heap)
      ;; else: use the same method as above to avoid recomputations
      (evalvarassign1 (car (car varassigns))
                      (cadr (car varassigns))
                      (cdr varassigns) ;;*** instead of evaluating it then store it in the env, we just store it directly.
                      expr
                      env
                      heap)))
;; input: variable (value/exception heap) varassigns expr env
;; output: evaluate expr in new env and heap (mutual recursion with evalvarassign)
(define (evalvarassign1 variable sem varassigns expr env heap)
  (evalvarassign varassigns expr (cons (list variable sem) env) heap))

;; translate applications to var-expressions as discussed in class
;; input: expr = (f ArgList)
(define (translate expr env)
  (translate1 (findfun (car expr) (cadr expr) env)))

;; env ((f ParamList) fDef)
(define (findfun fname args env)
  (if (and (list? (car (car env))) ;; here is a function
           (equal? (car (car (car env))) fname) ;; name matches
           (equal? (length (cadr (car (car env)))) (length args))) ;; arg size matches
      (list (cadr (car (car env)))  ;; parameter list
            args                    ;; arg list
            (cadr (car env)))       ;; body of the function to evaluate
      ;; else
      (findfun fname args (cdr env))))


(define (translate1 triplet)
  (list 'var (translate2 (car triplet) (cadr triplet)) (cadr (cdr triplet))))

;; input: paramlist, arglist, expression to evaluate
(define (translate2 paramlist arglist)
  (if (null? paramlist)
      '()
      (cons (list (car paramlist) (car arglist))
            (translate2 (cdr paramlist) (cdr arglist)))))

;; Now the pointer operations
;; deref
;; input: location and heap
;; output: value at location/exception paired with heap 
(define (derefhandler location heap)
    (if (null? heap)
        (list '(exception ooma) heap)
        (if (equal? (car (car heap)) location)
            (if (equal? (cadr (car heap)) 'free)
                (list '(exception fma) heap)
                (list (cadr (car heap)) heap))
            (mycons (car heap) (derefhandler location (cdr heap)))))) ;;; mycons will add the element to the second element of pair returned.

;; wref
;; input: location, value, heap
;; output: value written at location/exception heap
(define (wrefhandler location value heap)
  (if (list? value)
      (list value heap)
      (if (list? location)
          (list location heap)
          (if (null? heap)
              (list '(exception ooma) heap)
              (if (equal? (car (car heap)) location)
                  (if (equal? (cadr (car heap)) 'free)
                      (list '(exception fma) heap)
                      (list value (cons (list location value) (cdr heap))))
                  (mycons (car heap) (wrefhandler location value (cdr heap))))))))

(define (mycons el pair)
  (list (car pair) (cons el (cadr pair))))

;; input: location to free
;; output: location/exception heap
(define (freehandler location heap)
  (if (null? heap)
      (list '(exception ooma) heap)
      (if (equal? (car (car heap)) location)
          (if (equal? (cadr (car heap)) 'free)
              (list '(exception fma) heap)
              (list location (cons (list location 'free) (cdr heap))))
          (mycons (car heap) (freehandler location (cdr heap))))))

(define (refhandler value heap)
  (if (null? heap)
      (list '(exception oom) heap)
      (if (equal? (cadr (car heap)) 'free) ;; found free location
          (list (car (car heap)) ;; location
                (cons (list (car (car heap)) value) (cdr heap)))
          (mycons (car heap) (refhandler value (cdr heap))))))

;; input: location, value, heap
;; output: value written at location/exception heap

;; input: condition thenexpr elseexpr env heap
;; output: value/exception heap
;; same method as in evalarith
(define (ifthenelse condition thenexpr elseexpr env heap)
  (ifthenelse1 (evalcond condition env heap) thenexpr elseexpr env))

(define (ifthenelse1 semcond thenexpr elseexpr env)
  (if (list? (value semcond)) ;; that means exception
      semcond
      (if (value semcond) ;; condition is true
          (eval thenexpr env (heap semcond))
          (eval elseexpr env (heap semcond)))))

;; input: gt/lt/eq/and/or/not env heap
;; output: true/false/exception heap
;; same method as above: divide and conquer
(define (evalcond condition env heap)
  (if (equal? (car condition) 'not)
      (evalcond1 (car condition) (cadr condition) 'dummy env heap) ;; not just an one operand
      (evalcond1 (car condition) (cadr condition) (cadr (cdr condition)) env heap)))

(define (evalcond1 op operand1 operand2 env heap)
  (cond
   [ (or (equal? op 'gt)
         (equal? op 'lt)
         (equal? op 'eq))
     (evalcond2 op (eval operand1 env heap) operand2 env) ] ;; basic conditions: call evals
   [ else (evalcond2 op (evalcond operand1 env heap) operand2 env) ] ;; call evalconds
   ))

(define (evalcond2 op semoperand1 operand2 env)
  (if (list? (value semoperand1)) ;; exception
      semoperand1
      (if (equal? operand2 'dummy) ;; op is not
          (list (myapply op (value semoperand1) 'dummy) (heap semoperand1))
          (if (and (equal? op 'and) (not (value semoperand1))) ;;*** if op is 'and', and the first condition is false.
              (list false (heap semoperand1)) ;;*** instead of evaluating the following condition, we return false directly. 
              (if (and (equal? op 'or) (value semoperand1)) ;;*** if op is 'or', and the first condition is true.
                  (list true (heap semoperand1)) ;;*** instead of evaluating the following condition, we return true directly. 
                  (cond
                    [ (or (equal? op 'gt)
                          (equal? op 'lt)
                          (equal? op 'eq))
                      (evalcond3 op (value semoperand1) (eval operand2 env (heap semoperand1))) ] ;; basic conditions
                    [ else
                      (evalcond3 op (value semoperand1) (evalcond operand2 env (heap semoperand1))) ]
                    ))))))
                  

(define (evalcond3 op valoperand1 semoperand2)
  (if (list? (value semoperand2)) ;; exception
      semoperand2
      (list (myapply op valoperand1 (value semoperand2)) (heap semoperand2))))


(define (myapply op val1 val2)
  (cond
    [ (equal? op '+) (+ val1 val2) ]
    [ (equal? op '-) (- val1 val2) ]
    [ (equal? op '*) (* val1 val2) ]
    [ (equal? op 'gt) (> val1 val2) ]
    [ (equal? op 'lt) (< val1 val2) ]
    [ (equal? op 'eq) (equal? val1 val2) ]
    [ (equal? op 'and) (and val1 val2) ]
    [ (equal? op 'or) (or val1 val2) ]
    [ (equal? op 'not) (not val1) ]))  
                   



                  