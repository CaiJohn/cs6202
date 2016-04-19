;;; week-10-self-interpreter-with-solutions.scm
;;; was:
;;; week-10-self-interpreter.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 04 Nov 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-10.html

;;;;;;;;;;

;;; <expression> ::= <integer>
;;;                | <boolean>
;;;                | <string>
;;;                | <variable>
;;;                | (quote <value>)
;;;                | (time <expression>)
;;;                | (if <expression> <expression> <expression>)
;;;                | (cond {[<expression> <expression>]}* [else <expression>])
;;;                | (let ([<variable> <expression>]) <expression>)
;;;                | (letrec ([<variable> <lambda-abstraction>]) <expression>)
;;;                | <lambda-abstraction>
;;;                | (<expression>)
;;;                | (<expression> <expression>)
;;;                | (<expression> <expression> <expression>)
;;;                | (<expression> <expression> <expression> <expression>)
;;;                | (<expression> <expression> <expression> <expression> <expression>)

;;; <lambda-abstraction> ::= (lambda () <expression>)
;;;                        | (lambda (<variable>) <expression>)
;;;                        | (lambda (<variable> <variable>) <expression>)
;;;                        | (lambda (<variable> <variable> <variable>) <expression>)
;;;                        | (lambda (<variable> <variable> <variable> <variable>) <expression>)

;;; <integer> ::= ...any Scheme integer...
;;; <boolean> ::= ...any Scheme boolean...
;;; <string> ::= ...any Scheme string...
;;; <variable> ::= ...any Scheme symbol...
;;; <value> ::= ...any Scheme value...

;;; the initial environment contains at least all the procedures defined below

;;;;;;;;;;

(define Test-1
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate e)
		v)
	(printf "Test-1 error: ~s / ~s~n" v e))))

(define Test-2
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate (list representation-of-candidate (list 'quote e)))
		v)
	(printf "Test-2 error: ~s / ~s~n" v e))))

(define Test-3
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate  (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote e)))))
		v)
	(printf "Test-3 error: ~s / ~s~n" v e))))

;;;;;;;;;;

;;; Exercise 1
;;; ----------
;;; 
;;; * Define a test procedure ``Test-4`` that runs the four first layers of a
;;;   tower of interpreters.
;;; 
;;; * Define a test procedure ``Test-5`` that runs the five first layers of a
;;;   tower of interpreters.

(define Test-4
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate  (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote e)))))))
		v)
	(printf "Test-4 error: ~s / ~s~n" v e))))

(define Test-5
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate  (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote e)))))))))
		v)
	(printf "Test-5 error: ~s / ~s~n" v e))))

(define Test-6
  (lambda (candidate representation-of-candidate e v)
    (or (equal? (candidate  (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote (list representation-of-candidate (list 'quote e)))))))))))
		v)
	(printf "Test-6 error: ~s / ~s~n" v e))))

;;;;;;;;;;

;;; Exercise 2
;;; ----------
;;; 
;;; Define a test procedure ``make-Test`` that given a non-negative integer
;;; n, returns a test procedure that runs the n first layers of a tower of
;;; interpreters:
;;; 
;;; * applying ``make-Test`` to ``0`` should yield a procedure that is
;;;   equivalent to ``Test-0``;
;;; 
;;; * applying ``make-Test`` to ``1`` should yield a procedure that is
;;;   equivalent to ``Test-1``;
;;; 
;;; * applying ``make-Test`` to ``2`` should yield a procedure that is
;;;   equivalent to ``Test-2``;
;;; 
;;; * etc.

(define make-Test
  (lambda (n)
    (errorf 'make-Test "not implemented yet")))

;;;;;;;;;;

(define meta-test-interpret
  (lambda (test candidate representation-of-candidate)
    (begin
      (test candidate
            representation-of-candidate
	    1
	    1)
      (test candidate
            representation-of-candidate
	    #t
	    #t)
      (test candidate
            representation-of-candidate
	    "foo"
	    "foo")
      (test candidate
            representation-of-candidate
	    ''symbol
	    'symbol)
      (test candidate
            representation-of-candidate
	    '(time "things take")
	    "things take")
      (test candidate
            representation-of-candidate
	    '(if #t 1 2)
	    1)
      (test candidate
            representation-of-candidate
	    '(if 3 1 2)
	    1)
      (test candidate
            representation-of-candidate
	    '(if #f 1 2)
	    2)
      (test candidate
            representation-of-candidate
	    '(cond [#f 1] [#f 2] [else 3])
	    3)
      (test candidate
            representation-of-candidate
	    '(cond [#f 1] [#t 2] [else 3])
	    2)
      (test candidate
            representation-of-candidate
	    '((lambda () 2))
	    2)
      (test candidate
            representation-of-candidate
	    '((lambda (x) 2) 3)
	    2)
      (test candidate
            representation-of-candidate
	    '((lambda (x) x) 3)
	    3)
      (test candidate
            representation-of-candidate
	    '((lambda (x y) x) 3 4)
	    3)
      (test candidate
            representation-of-candidate
	    '((lambda (x y) y) 3 4)
	    4)
      (test candidate
            representation-of-candidate
	    '(letrec ([foo (lambda (x) x)]) 1)
	    1)
      (test candidate
            representation-of-candidate
	    '(letrec ([foo (lambda (x) x)]) (foo 1))
	    1)
      (test candidate
            representation-of-candidate
            '(letrec ([append (lambda (xs ys)
                                (cond
                                 [(null? xs)
                                  ys]
                                 [(pair? xs)
                                  (cons (car xs)
                                        (append (cdr xs) ys))]
                                 [else
                                  (errorf 'append
                                          "not a proper list: ~s"
                                          xs)]))])
               (append '() '(4 5 6)))
            '(4 5 6))
      ;;; add more here
      'done)))

;;;;;;;;;;

(define make-env-empty
  (lambda (name-of-the-interpreter)
    (lambda (x)
      (errorf name-of-the-interpreter
              "unbound variable: ~s"
              x))))

(define env-lookup
  (lambda (x env)
    (env x)))

(define env-extend-1
  (lambda (x v env)
    (lambda (y)
      (if (eqv? y x)
          v
          (env y)))))

;;;;;;;;;;

(define proper-list-of-fixed-length?
  (lambda (v n)
    (letrec ([visit (lambda (v n)
                      (if (= n 0)
                          (null? v)
                          (and (pair? v)
                               (visit (cdr v) (- n 1)))))])
      (if (and (integer? n)
               (>= n 0))
          (visit v n)
          (errorf 'proper-list-of-fixed-length
                  "not a non-negative integer: ~s"
                  n)))))

;;;;;;;;;;

;;; Predicates:

(define is-keyword?
  (lambda (x)
    (member x '(quote time if cond else let letrec lambda))))

(define is-integer?
  integer?)

(define is-boolean?
  boolean?)

(define is-string?
  string?)

(define is-variable?
  (lambda (v)
    (and (symbol? v)
         (not (is-keyword? v)))))

(define is-quote?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 2)
         (equal? (car v) 'quote))))

(define is-time?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 2)
         (equal? (car v) 'time))))

(define is-if?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 4)
         (equal? (car v) 'if))))

(define is-cond?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'cond))))

(define is-let?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'let))))

(define is-letrec?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'letrec))))

(define is-lambda-0?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'lambda)
         (proper-list-of-fixed-length? (cadr v) 0))))

(define is-lambda-1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'lambda)
         (proper-list-of-fixed-length? (cadr v) 1))))

(define is-lambda-2?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'lambda)
         (proper-list-of-fixed-length? (cadr v) 2))))

(define is-lambda-3?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'lambda)
         (proper-list-of-fixed-length? (cadr v) 3))))

(define is-lambda-4?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'lambda)
         (proper-list-of-fixed-length? (cadr v) 4))))

(define is-application-0?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 1)
         (let ([w (car v)])
           (if (symbol? w)
               (not (is-keyword? w))
               #t)))))

(define is-application-1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 2)
         (let ([w (car v)])
           (if (symbol? w)
               (not (is-keyword? w))
               #t)))))

(define is-application-2?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (let ([w (car v)])
           (if (symbol? w)
               (not (is-keyword? w))
               #t)))))

(define is-application-3?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 4)
         (let ([w (car v)])
           (if (symbol? w)
               (not (is-keyword? w))
               #t)))))

(define is-application-4?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 5)
         (let ([w (car v)])
           (if (symbol? w)
               (not (is-keyword? w))
               #t)))))

;;;;;;;;;;

;;; Accessors:

(define integer_0
  (lambda (e)
    e))

(define boolean_0
  (lambda (e)
    e))

(define string_0
  (lambda (e)
    e))

(define variable_0
  (lambda (e)
    e))

(define quote_1
  (lambda (v)
    (list-ref v 1)))

(define time_1
  (lambda (v)
    (list-ref v 1)))

(define if_1
  (lambda (v)
    (list-ref v 1)))

(define if_2
  (lambda (v)
    (list-ref v 2)))

(define if_3
  (lambda (v)
    (list-ref v 3)))

(define cond_1
  (lambda (v)
    (list-ref v 1)))

(define cond_rest
  (lambda (v)
    (list-tail v 2)))

(define lambda_1
  (lambda (v)
    (list-ref v 1)))

(define lambda_2
  (lambda (v)
    (list-ref v 2)))

(define application_0
  (lambda (v)
    (list-ref v 0)))

(define application_1
  (lambda (v)
    (list-ref v 1)))

(define application_2
  (lambda (v)
    (list-ref v 2)))

(define application_3
  (lambda (v)
    (list-ref v 3)))

(define application_4
  (lambda (v)
    (list-ref v 4)))

(define let_1
  (lambda (v)
    (list-ref v 1)))

(define let_2
  (lambda (v)
    (list-ref v 2)))

(define letrec_1
  (lambda (v)
    (list-ref v 1)))

(define letrec_2
  (lambda (v)
    (list-ref v 2)))

(define clause_1
  (lambda (v)
    (list-ref v 0)))

(define clause_2
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; Environment:

(define make-env-empty
  (lambda (name)
    (lambda (x)
      (errorf name
              "unbound variable: ~s"
              x))))

(define env-lookup
  (lambda (x env)
    (env x)))

(define env-extend-1
  (lambda (x v env)
    (lambda (y)
      (if (eqv? y x)
          v
          (env y)))))

(define make-env-init
  (lambda (name predefined-variables predefined-values)
    (letrec ([visit (lambda (ns vs)
                      (if (null? ns)
                          (env-extend-1 'predefined-variables
                                        predefined-variables
                                        (env-extend-1 'predefined-values
                                                      predefined-values
                                                      (make-env-empty name)))
                          (env-extend-1 (car ns)
                                        (car vs)
                                        (visit (cdr ns)
                                               (cdr vs)))))])
      (visit predefined-variables predefined-values))))

(define predefined-variables
  '(make-env-empty
    env-lookup
    env-extend-1
    make-env-init
    is-integer?
    integer_0
    is-boolean?
    boolean_0
    is-string?
    string_0
    is-variable?
    variable_0
    is-quote?
    quote_1
    is-time?
    time_1
    is-if?
    if_1
    if_2
    if_3
    cond_1
    cond_rest
    clause_1
    clause_2
    is-cond?
    proper-list-of-fixed-length?
    null?
    equal?
    car
    errorf
    pair?
    cdr
    is-let?
    let_1
    let_2
    is-letrec?
    letrec_1
    letrec_2
    lambda_1
    lambda_2
    env-extend-1
    is-lambda-0?
    is-lambda-1?
    is-lambda-2?
    is-lambda-3?
    is-lambda-4?
    cons
    is-application-0?
    is-application-1?
    is-application-2?
    is-application-3?
    is-application-4?
    application_0
    application_1
    application_2
    application_3
    application_4
    list-ref
    list-tail
    +
    ;;; etc.
    ))

(define predefined-values
  (list make-env-empty
        env-lookup
        env-extend-1
        make-env-init
        is-integer?
        integer_0
        is-boolean?
        boolean_0
        is-string?
        string_0
        is-variable?
        variable_0
        is-quote?
        quote_1
        is-time?
        time_1
        is-if?
        if_1
        if_2
        if_3
        cond_1
        cond_rest
        clause_1
        clause_2
        is-cond?
        proper-list-of-fixed-length?
        null?
        equal?
        car
        errorf
        pair?
        cdr
        is-let?
        let_1
        let_2
        is-letrec?
        letrec_1
        letrec_2
        lambda_1
        lambda_2
        env-extend-1
        is-lambda-0?
        is-lambda-1?
        is-lambda-2?
        is-lambda-3?
        is-lambda-4?
        cons
        is-application-0?
        is-application-1?
        is-application-2?
        is-application-3?
        is-application-4?
        application_0
        application_1
        application_2
        application_3
        application_4
        list-ref
        list-tail
        +
        ;;; etc.
        ))

;;;;;;;;;;

;;; The interpreter:

(define interpret
  ;;'
  (lambda (e)
    (letrec ([eval
              (lambda (e env)
                (cond
                  [(is-integer? e)
                   (integer_0 e)]
                  [(is-boolean? e)
                   (boolean_0 e)]
                  [(is-string? e)
                   (string_0 e)]
                  [(is-variable? e)
                   (env-lookup (variable_0 e) env)]
                  [(is-quote? e)
                   (quote_1 e)]
                  [(is-time? e)
                   (time (eval (time_1 e) env))]
                  [(is-if? e)
                   (if (eval (if_1 e) env)
                       (eval (if_2 e) env)
                       (eval (if_3 e) env))]
                  [(is-cond? e)
                   (letrec ([visit
                             (lambda (c cs)
                               (cond
                                 [(null? cs)
                                  (eval (clause_2 c) env)]
                                 [(eval (clause_1 c) env)
                                  (eval (clause_2 c) env)]
                                 [else
                                  (visit (car cs) (cdr cs))]))])
                     (visit (cond_1 e) (cond_rest e)))]
                  [(is-lambda-0? e)
                   (let ([lambda-body (lambda_2 e)])
                     (lambda ()
                       (eval lambda-body env)))]
                  [(is-lambda-1? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (lambda (v1)
                           (eval lambda-body (env-extend-1 x1 v1 env))))))]
                  [(is-lambda-2? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (let ([x2 (list-ref lambda-formals 1)])
                           (lambda (v1 v2)
                             (eval lambda-body (env-extend-1 x2 v2 (env-extend-1 x1 v1 env))))))))]
                  [(is-lambda-3? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (let ([x2 (list-ref lambda-formals 1)])
                           (let ([x3 (list-ref lambda-formals 2)])
                             (lambda (v1 v2 v3)
                               (eval lambda-body (env-extend-1 x3 v3 (env-extend-1 x2 v2 (env-extend-1 x1 v1 env))))))))))]
                  [(is-lambda-4? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (let ([x2 (list-ref lambda-formals 1)])
                           (let ([x3 (list-ref lambda-formals 2)])
                             (let ([x4 (list-ref lambda-formals 3)])
                               (lambda (v1 v2 v3 v4)
                                 (eval lambda-body (env-extend-1 x4 v4 (env-extend-1 x3 v3 (env-extend-1 x2 v2 (env-extend-1 x1 v1 env))))))))))))]
                  [(is-application-0? e)
                   (let ([operator (application_0 e)])
                     ((eval operator env)))]
                  [(is-application-1? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       ((eval operator env)
                        (eval operand1 env))))]
                  [(is-application-2? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       (let ([operand2 (application_2 e)])
                         ((eval operator env)
                          (eval operand1 env)
                          (eval operand2 env)))))]
                  [(is-application-3? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       (let ([operand2 (application_2 e)])
                         (let ([operand3 (application_3 e)])
                           ((eval operator env)
                            (eval operand1 env)
                            (eval operand2 env)
                            (eval operand3 env))))))]
                  [(is-application-4? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       (let ([operand2 (application_2 e)])
                         (let ([operand3 (application_3 e)])
                           (let ([operand4 (application_4 e)])
                             ((eval operator env)
                              (eval operand1 env)
                              (eval operand2 env)
                              (eval operand3 env)
                              (eval operand4 env)))))))]
                  [(is-let? e)
                   (let ([let-headers (let_1 e)])
                     (let ([let-header (car let-headers)])
                       (let ([let-var (clause_1 let-header)])
                         (let ([let-exp (clause_2 let-header)])
                           (let ([let-body (let_2 e)])
                             (eval let-body
                                   (env-extend-1 let-var
                                                 (eval let-exp env)
                                                 env)))))))]
                  [(is-letrec? e)
                   (let ([letrec-headers (letrec_1 e)])
                     (let ([letrec-header (car letrec-headers)])
                       (let ([letrec-var (clause_1 letrec-header)])
                         (let ([letrec-lam (clause_2 letrec-header)])
                           (let ([letrec-body (letrec_2 e)])
                             (letrec ([env-rec
                                       (lambda (x)
                                         (if (equal? x letrec-var)
                                             (eval letrec-lam env-rec)
                                             (env x)))])
                               (eval letrec-body env-rec)))))))]
                  [else
                   (errorf 'interpret
                           "unrecognized input: ~s"
                           e)]))])
      (eval e (make-env-init 'interpret predefined-variables predefined-values)))))

;;;;;;;;;;

;;; The representation of the interpreter:

(define representation-of-interpret
  '
  (lambda (e)
    (letrec ([eval
              (lambda (e env)
                (cond
                  [(is-integer? e)
                   (integer_0 e)]
                  [(is-boolean? e)
                   (boolean_0 e)]
                  [(is-string? e)
                   (string_0 e)]
                  [(is-variable? e)
                   (env-lookup (variable_0 e) env)]
                  [(is-quote? e)
                   (quote_1 e)]
                  [(is-time? e)
                   (time (eval (time_1 e) env))]
                  [(is-if? e)
                   (if (eval (if_1 e) env)
                       (eval (if_2 e) env)
                       (eval (if_3 e) env))]
                  [(is-cond? e)
                   (letrec ([visit
                             (lambda (c cs)
                               (cond
                                 [(null? cs)
                                  (eval (clause_2 c) env)]
                                 [(eval (clause_1 c) env)
                                  (eval (clause_2 c) env)]
                                 [else
                                  (visit (car cs) (cdr cs))]))])
                     (visit (cond_1 e) (cond_rest e)))]
                  [(is-lambda-0? e)
                   (let ([lambda-body (lambda_2 e)])
                     (lambda ()
                       (eval lambda-body env)))]
                  [(is-lambda-1? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (lambda (v1)
                           (eval lambda-body (env-extend-1 x1 v1 env))))))]
                  [(is-lambda-2? e)
                   (let ([lambda-formals (lambda_1 e)])
                     (let ([lambda-body (lambda_2 e)])
                       (let ([x1 (list-ref lambda-formals 0)])
                         (let ([x2 (list-ref lambda-formals 1)])
                           (lambda (v1 v2)
                             (eval lambda-body (env-extend-1 x2 v2 (env-extend-1 x1 v1 env))))))))]
                  [(is-lambda-3? e)
                   (lambda (v1 v2 v3)
                     (eval (lambda_2 e)
                           (env-extend-1 (list-ref (lambda_1 e) 2)
                                         v3
                                         (env-extend-1 (list-ref (lambda_1 e) 1)
                                                       v2
                                                       (env-extend-1 (list-ref (lambda_1 e) 0)
                                                                     v1
                                                                     env)))))]
                  [(is-lambda-4? e)
                   (lambda (v1 v2 v3 v4)
                     (eval (lambda_2 e)
                           (env-extend-1 (list-ref (lambda_1 e) 3)
                                         v4
                                         (env-extend-1 (list-ref (lambda_1 e) 2)
                                                       v3
                                                       (env-extend-1 (list-ref (lambda_1 e) 1)
                                                                     v2
                                                                     (env-extend-1 (list-ref (lambda_1 e) 0)
                                                                                   v1
                                                                                   env))))))]
                  [(is-application-0? e)
                   (let ([operator (application_0 e)])
                     ((eval operator env)))]
                  [(is-application-1? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       ((eval operator env)
                        (eval operand1 env))))]
                  [(is-application-2? e)
                   (let ([operator (application_0 e)])
                     (let ([operand1 (application_1 e)])
                       (let ([operand2 (application_2 e)])
                         ((eval operator env)
                          (eval operand1 env)
                          (eval operand2 env)))))]
                  [(is-application-3? e)
                   ((eval (application_0 e) env)
                    (eval (application_1 e) env)
                    (eval (application_2 e) env)
                    (eval (application_3 e) env))]
                  [(is-application-4? e)
                   ((eval (application_0 e) env)
                    (eval (application_1 e) env)
                    (eval (application_2 e) env)
                    (eval (application_3 e) env)
                    (eval (application_4 e) env))]
                  [(is-let? e)
                   (let ([let-headers (let_1 e)])
                     (let ([let-header (car let-headers)])
                       (let ([let-var (clause_1 let-header)])
                         (let ([let-exp (clause_2 let-header)])
                           (let ([let-body (let_2 e)])
                             (eval let-body
                                   (env-extend-1 let-var
                                                 (eval let-exp env)
                                                 env)))))))]
                  [(is-letrec? e)
                   (let ([letrec-headers (letrec_1 e)])
                     (let ([letrec-header (car letrec-headers)])
                       (let ([letrec-var (clause_1 letrec-header)])
                         (let ([letrec-lam (clause_2 letrec-header)])
                           (let ([letrec-body (letrec_2 e)])
                             (letrec ([env-rec
                                       (lambda (x)
                                         (if (equal? x letrec-var)
                                             (eval letrec-lam env-rec)
                                             (env x)))])
                               (eval letrec-body env-rec)))))))]
                  [else
                   (errorf 'interpret
                           "unrecognized input: ~s"
                           e)]))])
      (eval e (make-env-init 'interpret predefined-variables predefined-values)))))

;;;;;;;;;;

;;; end of week-10-self-interpreter-with-solutions.scm

"week-10-self-interpreter-with-solutions.scm"
