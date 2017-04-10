#lang racket
(require racket/trace)
(require "program.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval
;; input: program-expression, environment as list of pairs (variable value)
;; output: '(Cannot Evaluate) or a number


(define (eval expr env heap)
  (if (caneval env)      ;; the environment might end up with a pair (variable '(Cannot Evaluate))
      (cond
        [ (equal? (checkexception expr) '(exception fma)) (cons '(exception fma) (list heap))]
        [ (equal? (checkexception expr) '(exception ooma)) (cons '(exception ooma) (list heap))]
        [ (equal? (checkexception expr) '(exception oom)) (cons '(exception oom) (list heap))]
        [ (number? expr) (cons expr (list heap)) ]  ;; number
        [ (symbol? expr) (cons (findvalue expr env) (list heap)) ] ;; sigma(x)

        ;; implementation of the semantics of variable expression
        [ (equal? (car expr) 'var) (evalvarassign (cadr expr) (cadr (cdr expr)) env heap) ]

        ;; implementation of the semantics of FExpr
        [ (equal? (car expr) 'fun) (eval (cadr (cdr expr)) (cons (cadr expr) env) heap) ]

        ;; implementation of the semantics of Apply
        ;; utter disregard for efficiency - searching the same environment three times using the same key
        ;; But this keeps it clean
        ;; We are going to translate the apply semantics to varexpression semantics
        ;; [[(apply (f (ArgList)))]]_env = [[(var Param-ArgValue list  FDef]]_staticenv
        [ (equal? (car expr) 'apply) (eval (list 'var
                                                 (paramargs (findfunparams (car (cadr expr))  ;; function name
                                                                           (length (cadr (cadr expr))) ;; number of params
                                                                           env)              ;; current environment
                                                            ;; findfunparams returns parameters of the function
                                                            (cadr (cadr expr)) ;; expressions representing arguments
                                                            env heap)
                                                 ;; paramargs returns the list of variable-value pairs
                                                 
                                                 (findfundef (car (cadr expr)) (length (cadr (cadr expr))) env)) ;; definition of the function
                                           
                                           (staticenv (car (cadr expr)) (length (cadr (cadr expr))) env) ;; static environment
                                           heap ) ]
 
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]

        ;;implementation of the semantics of DREF
        [ (equal? (car expr) 'deref) (cons (evaldref (findval (car (eval (second expr) env heap))) (second (eval (second expr) env heap))) (list (second (eval (second expr) env heap))))]

        ;;implementation of the semantics of WREF
        [ (equal? (car expr) 'wref) (evalwref (car (eval (second expr) env heap)) (car (eval (third expr) env heap)) (second (eval (third expr) env heap))) ]

        ;;implementation of the semantics of REF
        [ (equal? (car expr) 'ref) (evalref (car (eval (second expr) env heap)) (second (eval (second expr) env heap)))]

        ;;implementation of the semantics of FREE
        [ (equal? (car expr) 'free) (evalfree (car (eval (second expr) env heap)) (second (eval (second expr) env heap)))]

        ;; ifthenelse function
        [ else  (ifthenelse (car (evalcond (car expr) env heap)) 
                            (cadr expr)
                            (cadr (cdr expr)) env (second (evalcond (car expr) env heap))) ]
        )
      '(Cannot Evaluate)
      ))

;;evaldref
;;input: location address and the heap
;;output: exceptions || values stored at the location address in the heap
;;example: (evaldref '1 '((1 free))) : '(exception fma)
;;example: (evaldref '1 '()) : '(exception ooma)
;;example: (evaldref '1 '((1 100))) : 100
(define (evaldref location heap)
  (if (not (number? location))
      location
      (if (null? heap)
          '(exception ooma)
          (if (equal? location (car (car heap)))
              (if (equal? 'free (second (car heap)))
                  '(exception fma)
                  (second (car heap)))
              (evaldref location (cdr heap))))))
              
;;evalwref
;;input: location address, value to store and the heap
;;output: exceptions || values stored at the location address in the heap (with the new heap behind them)
;;example: (evalwref '1 '23 '((1 free))) : '((exception fma) ((1 free)))
;;example: (evalwref '2 '23 '((1 free))) : '((exception ooma) ((1 free)))
;;example: (evalwref '1 '23 '((1 10))) : '(23 ((1 23)))
(define (evalwref location value heap)
  (if (not (number? location))
      (cons location (list heap))
      (if (not (number? value))
          (cons value (list heap))
          (if (equal? '(exception fma) (write location value heap))
              (cons '(exception fma) (list heap))
              (if (equal? '(exception ooma) (write location value heap))
                  (cons '(exception ooma) (list heap))
                  (cons value (list(write location value heap))))))))

;;helper function to write value at the location in heap (location must have value already!)
;;input: location address, value to store and the heap
;;output: exceptions || a new heap with value stored at location
;;example: (write '1 '1 '((1 free))) : '(exception fma)
;;example: (write '2 '1 '((1 free))) : '(exception ooma)
;;example: (write '1 '1 '((1 10))) : '((1 1))
(define (write location value heap)
  (if (null? heap)
      '(exception ooma)
      (if (equal? location (car (car heap)))
          (if (equal? 'free (second (car heap)))
                  '(exception fma)
                  (cons (list (car (car heap)) value) (cdr heap)))
              (if (equal? (write location value (cdr heap)) '(exception ooma)) 
              '(exception ooma)
              (if (equal? (write location value (cdr heap)) '(exception fma))
                  '(exception fma)
                  (cons (car heap) (write location value (cdr heap))))))))

;;evalref
;;input: value to store and the heap
;;output: exceptions || location that values stored at in the heap (with the new heap behind them)
;;example: (evalref '1 '((1 1))) : '((exception oom) ((1 1)))
;;example: (evalref '2 '((1 free))) : '(1 ((1 2)))
(define (evalref value heap)
  (if (not (number? value))
      (cons value (list heap))
      (if (equal? '(exception oom) (find heap))
          (cons '(exception oom) (list heap))
          (cons (find heap) (list (writefree (find heap) value heap))))))

;;helper function writetofree: write the value to a free location
;;input: location address, value to store and the heap
;;output: a new heap with value stored
;;example: (writefree '1 '1 '((1 free))) : '((1 1))
;;example: (writefree '2 '20 '((2 free))) : '((2 20))
(define (writefree location value heap)
  (if (equal? location (car (car heap)))
      (cons (list (car (car heap)) value) (cdr heap))
      (cons (car heap) (writefree location value (cdr heap)))))

;;helper function to find the free location address
;;input: the heap
;;output: exception if no free location || location
;;example: (find '((1 1))) : '(exception oom)
;;example: (find '((1 free))) : 1
;;example: (find '((1 21) (2 free) (3 30))) : 2
(define (find heap)
  (if (null? heap)
      '(exception oom)
      (if (equal? 'free (second (car heap)))
          (car (car heap))
          (find (cdr heap)))))

;;evalfree
;;input: location address and the heap
;;output: exceptions || location that the value is freed in the heap (with the new heap behind them!)
;;example: (evalfree '1 '((1 free))) : '((exception fma) ((1 free)))
;;example: (evalfree '2 '((1 free))) : '((exception ooma) ((1 free)))
;;example: (evalfree '1 '((1 1))) : '(1 ((1 free)))
(define (evalfree location heap)
  (if (not (number? location))
      (cons location (list heap))
      (if (equal? '(exception fma) (write location 'free heap))
              (cons '(exception fma) (list heap))
              (if (equal? '(exception ooma) (write location 'free heap))
                  (cons '(exception ooma) (list heap))
                  (cons location (list(write location 'free heap)))))))

;; input: variable, environment
;; output: value to which the variable is mapped to in the environment
;;         It can be '(Cannot Evaluate) 
(define (findvalue x env)
  (if (null? env)  
      '(Cannot Evaluate)
      (if (equal? (car (car env)) x)
          (cadr (car env))
          (findvalue x (cdr env)))))

;; input: environment
;; output: true, if no variable is mapped to '(Cannot Evaluate)
;;         false, otherwise
;; Exercise: implement in another way, where it does not depend on '(Cannot Evaluate)
(define (caneval env)
  (if (null? env)
      true
      (and (not (equal? (cadr (car env)) '(Cannot Evaluate)))
           (caneval (cdr env)))))

;; input: list of (variable expression), expr to evaluate, environment
;; output: evaluate expr in some environment

(define (evalvarassign varassigns expr env heap)
  (if (null? varassigns)  ;; no variable expression pair, 
      (eval expr env heap)     ;; then just evaluate the expression in the current environment
      ;; else
      ;; recursive call with the suffix of varassigns, with the same expr
      ;; in the environment constructed by cons-ing (variable evaluation of expression)
      ;; to the given environment env.
      (evalvarassign (cdr varassigns)
                     expr
                     (cons (list (car (car varassigns))
                                 (car (eval (cadr (car varassigns)) env heap)))
                           env) (second (eval (cadr (car varassigns)) env heap)))))

;; is op arithmatic operation
(define (arithop op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '\))))

;; input: arithoperator, expr-operand1, expr-operand2, env
;; output: '(Cannot Evaluate) or some number
;; used: myapply 
(define (evalarith op expr1 expr2 env heap)
 (myapply op (eval expr1 env heap) (eval  expr2 env (second (eval expr1 env heap))) (second (eval  expr2 env (second (eval expr1 env heap))))))

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2 env heap)
  (if (checkexception condition)
      (cons condition (list heap))
      (if condition
          (cons (car (eval expr1 env heap)) (list (second (eval expr1 env heap))))
          (cons (car (eval expr2 env heap)) (list (second (eval expr2 env heap)))))))

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env heap)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (car (eval (cadr condexpr) env heap)) (eval (cadr (cdr condexpr)) env heap) (second (eval (cadr condexpr) env heap)) )]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (car (eval (cadr condexpr) env heap)) (eval (cadr (cdr condexpr)) env heap) (second (eval (cadr condexpr) env heap)) ) ]

    [ (equal? (car condexpr) 'eq)
      (myapply 'eq (car (eval (cadr condexpr) env heap)) (eval (cadr (cdr condexpr)) env heap) (second (eval (cadr condexpr) env heap))) ]
    
    [ (equal? (car condexpr) 'and)
      (myapply 'and (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env) heap) ]

    [ (equal? (car condexpr) 'or)
      (myapply 'or (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env) heap) ]

    [ (equal? (car condexpr) 'not)
      (myapply 'not (evalcond (cadr condexpr) env heap)
               false heap) ] ;; dummy
    ))

;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2 heap)
  (cond
    [ (or (equal? (checkexception val1) '(exception fma)) (equal? (checkexception val2) '(exception fma))) (cons '(exception fma) (list heap))]
    [ (or (equal? (checkexception val1) '(exception ooma)) (equal? (checkexception val2) '(exception ooma))) (cons '(exception ooma) (list heap))]
    [ (or (equal? (checkexception val1) '(exception oom)) (equal? (checkexception val2) '(exception oom))) (cons '(exception oom) (list heap))]
    [ (equal? op '+) (cons (+ (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op '-) (cons (- (findval val1) (findval val2)) (list heap))]
    [ (equal? op '*) (cons (* (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op 'gt) (cons (> (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op 'lt) (cons (< (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op 'eq) (cons (equal? (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op 'and) (cons (and (findval val1) (findval val2)) (list heap))]
    [ (equal? op 'or) (cons (or (findval val1) (findval val2)) (list heap)) ]
    [ (equal? op 'not) (cons (not (findval val1)) (list heap)) ]))

;;helper function to find the value in a list contain value and heap
;;input: a list contain value and heap
;;output: exceptions || value
;;example: (findval '((exception fma) ((1 free) (2 2) (3 3)))) : '(exception fma)
;;example: (findval '((exception ooma) ((1 free) (2 2) (3 3)))) : '(exception ooma)
;;example: (findval '((exception oom) ((1 free) (2 2) (3 3)))) : '(exception oom)
;;example: (findval '(5 ((1 free) (2 2) (3 3)))) : 5
(define (findval val)
  (if (not (list? val))
      val
      (if (or (equal? val '(exception fma)) (equal? val '(exception ooma)) (equal? val '(exception oom)) )
          val
          (findval (car val)))))

;;helper function to check if the value in the list contains value and heap is exception
;;input: a list contain value and heap
;;output: exceptions || false
;;example: (checkexception '((exception fma) ((1 free) (2 2) (3 3)))) : '(exception fma)
;;example: (checkexception '((exception ooma) ((1 free) (2 2) (3 3)))) : '(exception ooma)
;;example: (checkexception '((exception oom) ((1 free) (2 2) (3 3)))) : '(exception oom)
;;example: (checkexception '(5 ((1 free) (2 2) (3 3)))) : #f
(define (checkexception val)
  (if (not (list? val))
      false
      (if (equal? val '(exception fma))
          '(exception fma)
          (if (equal? val '(exception ooma))
              '(exception ooma)
              (if (equal? val '(exception oom))
                  '(exception oom)
                  (checkexception (car val)))))))

;; Functions added for the assignment 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (paramargs (x1 x2 .... xn) (e1 e2 .... en) env) = ((x1 e1val) (x2 e2val) ... (xn enval))
;; input: list of variables
;;        list of expressions
;;        environment
;; output: list of pairs of variable-expressionvalue
(define (paramargs paramlist exprlist env heap)
  (if (null? paramlist)
      '()
      (cons (list (car paramlist) (car (eval (car exprlist) env heap)))
            (paramargs (cdr paramlist) (cdr exprlist) env heap))))

;; find the function parameters
;; input: function name and arg-length
;;        env
;; output: list of function parameters
(define (findfunparams fname paramlength env)
  (if (and (list? (car (car env)))   ;; is a function definition
           (equal? (car (car (car env))) fname)  ;; name matches
           (equal? (length (cadr (car (car env)))) paramlength)) ;; paramlength matchs
      ;; 
      (cadr (car (car env)))   ;; return the list of parameters
      (findfunparams fname paramlength (cdr env)))) ;; else keep looking

;; Same as above: just return the definition of the function
(define (findfundef fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      ;; 
      (cadr (car env)) ;; return the definition of the function
      (findfundef fname paramlength (cdr env)))) ;; else keep looking

;; Given an environment; generate the static environment corresponding
;; for a function
;; same as above again
(define (staticenv fname paramlength env)
  (if (and (list? (car (car env)))
           (equal? (car (car (car env))) fname)
           (equal? (length (cadr (car (car env)))) paramlength))
      env ;; return the environment at the time of definition
      (staticenv fname paramlength (cdr env)))) ;; else keep looking