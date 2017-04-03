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
        [ (equal? expr '(exception fma)) (cons '(exception fma) (list heap))]
        [ (equal? expr '(exception ooma)) (cons '(exception ooma) (list heap))]
        [ (equal? expr '(exception oom)) (cons '(exception oom) (list heap))]
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
                                                            env)
                                                 ;; paramargs returns the list of variable-value pairs
                                                 
                                                 (findfundef (car (cadr expr)) (length (cadr (cadr expr))) env)) ;; definition of the function
                                           
                                           (staticenv (car (cadr expr)) (length (cadr (cadr expr))) env) ;; static environment
                                           heap ) ]
 
        ;; same as before with the arithmatic operations: environment is added
        [ (arithop (car expr)) (evalarith (car expr) (cadr expr) (cadr (cdr expr)) env heap) ]

        ;;DREF
        [ (equal? (car expr) 'deref) (evaldref (car (eval (second expr) env heap)) (second (eval (second expr) env heap)) (second (eval (second expr) env heap)))]

        ;;WREF
        [ (equal? (car expr) 'wref) (evalwref (car (eval (second expr) env heap)) (car (eval (third expr) env heap)) (second (eval (third expr) env heap))) ]

        ;;ref
        [ (equal? (car expr) 'ref) (evalref (car (eval (second expr) env heap)) (second (eval (second expr) env heap)))]

        ;;free
        [ (equal? (car expr) 'free) (evalfree (car (eval (second expr) env heap)) (second (eval (second expr) env heap)))]

        ;; ifthenelse function
        [ else  (ifthenelse (evalcond (car expr) env) 
                            (cadr expr)
                            (cadr (cdr expr)) env) ]
        )
      '(Cannot Evaluate)
      ))

;;evaldref
(define (evaldref location heap h)
  (if (not (number? location))
      location
      (if (null? heap)
          '(exception ooma)
          (if (equal? location (car (car heap)))
              (if (equal? 'free (second (car heap)))
                  '(exception fma)
                  (cons (second (car heap)) (list h)))
              (evaldref location (cdr heap) h)))))
              

;;evalwref
(define (evalwref location value heap)
  (if (not (number? location))
      location
      (if (not (number? value))
          value
          (if (equal? '(exception fma) (write location value heap))
              '(exception fma)
              (if (equal? '(exception ooma) (write location value heap))
                  '(exception ooma)
                  (cons value (list(write location value heap))))))))

;;writeheap
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


;;ref
(define (evalref value heap)
  (if (not (number? value))
      value
      (if (equal? '(exception oom) (find heap))
          '(exception oom)
          (cons (find heap) (list (writefree (find heap) value heap))))))

;;writetofree
(define (writefree location value heap)
  (if (equal? location (car (car heap)))
      (cons (list (car (car heap)) value) (cdr heap))
      (cons (car heap) (writefree location value (cdr heap)))))
;;find free
(define (find heap)
  (if (null? heap)
      '(exception oom)
      (if (equal? 'free (second (car heap)))
          (car (car heap))
          (find (cdr heap)))))
              
(define (evalfree location heap)
  (if (not (number? location))
      location
      (if (equal? '(exception fma) (write location 'free heap))
              '(exception fma)
              (if (equal? '(exception ooma) (write location 'free heap))
                  '(exception ooma)
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
                                 (eval (cadr (car varassigns)) env heap))
                           env) heap)))

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
 (myapply op (eval expr1 env heap) (eval  expr2 env heap) heap))

;; input: true/false, '(Cannot Evaluate) expression values
;; output: '(Cannot Evaluate) or expression values
;;         expression values can be '(Cannot Evaluate)
(define (ifthenelse condition expr1 expr2 env heap)
  (if (equal? condition '(Cannot Evaluate))
      '(Cannot Evaluate)
      (if condition
          (eval expr1 env heap)
          (eval expr2 env heap))))

;; input: conditions of the form (gt/lt/eq expr1 expr2), (or/and cond1 cond2), (not cond)
;; output: true/false, '(Cannot Evaluate)
;; used: myapply
(define (evalcond condexpr env heap)
  (cond
    [ (equal? (car condexpr) 'gt)
      (myapply 'gt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap) heap) ]
    
    [ (equal? (car condexpr) 'lt)
      (myapply 'lt (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap) heap) ]

    [ (equal? (car condexpr) 'eq)
      (myapply 'eq (eval (cadr condexpr) env heap) (eval (cadr (cdr condexpr)) env heap) heap) ]
    
    [ (equal? (car condexpr) 'and)
      (myapply 'and (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env) heap) ]

    [ (equal? (car condexpr) 'or)
      (myapply 'or (evalcond (cadr condexpr) env)
               (evalcond (cadr (cdr condexpr)) env) heap) ]

    [ (equal? (car condexpr) 'not)
      (myapply 'not (evalcond (cadr condexpr) env heap)
               false) ] ;; dummy
    ))


;; input: some operator, arithmatic or conditional
;;        operand-values for the operator
;; output: '(Cannot Evaluate) or number or boolean 
(define (myapply op val1 val2 heap)
  (cond
    [ (or (equal? (findval2 val1) '(exception fma)) (equal? (findval2 val2) '(exception fma))) (cons '(exception fma) (list heap))]
    [ (or (equal? (findval2 val1) '(exception ooma)) (equal? (findval2 val2) '(exception fma))) (cons '(exception ooma) (list heap))]
    [ (or (equal? (findval2 val1) '(exception oom)) (equal? (findval2 val2) '(exception fma))) (cons '(exception oom) (list heap))]
    [ (equal? op '+) (+ (findval val1) (findval val2)) ]
    [ (equal? op '-) (- (findval val1) (findval val2)) ]
    [ (equal? op '*) (* (findval val1) (findval val2)) ]
    [ (equal? op 'gt) (> (findval val1) (findval val2)) ]
    [ (equal? op 'lt) (< (findval val1) (findval val2)) ]
    [ (equal? op 'eq) (equal? (findval val1) (findval val2)) ]
    [ (equal? op 'and) (and (findval val1) (findval val2)) ]
    [ (equal? op 'or) (or (findval val1) (findval val2)) ]
    [ (equal? op 'not) (not (findval val1)) ]))
(trace myapply)
(define (findval val)
  (if (not (list? val))
      val
      (findval (car val))))
(define (findval2 val)
  (if (not (list? val))
      val
      (if (not (list? (car val)))
          val
          (findval (car val)))))
 (trace findval2)
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
      (cons (list (car paramlist) (eval (car exprlist) env heap))
            (paramargs (cdr paramlist) (cdr exprlist) env))))

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