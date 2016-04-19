;;; week-8-a-syntax-checker-for-Scheme-part-2.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-8-a-syntax-checker-for-Scheme-part-2.html

;;;;;;;;;;

(define the-current-well-formed-thing
  "not initialized yet")

;;; returns #t if all the well-formed things are deemed well formed
;;; returns #f if at least one of the well-formed things is deemed ill formed
;;; in case of hard error, the faulty thing is bound to the-current-well-formed-thing
(define test-well-formed-things
  (lambda (check things)
    (letrec ([visit (lambda (things b)
                      (cond
                        [(null? things)
                         (begin
                           (set! the-current-well-formed-thing "not initialized yet")
                           b)]
                        [(pair? things)
                         (begin
                           (set! the-current-well-formed-thing (car things))
                           (if (check (car things))
                               (visit (cdr things) b)
                               (begin
                                 (printf "improperly rejected: ~s~n"
                                         (car things))
                                 (visit (cdr things) #f))))]
                        [else
                         (errorf 'test-well-formed-things
                                 "ill-formed list: ~s"
                                 things)]))])
      (visit things #t))))

(define well-formed-definitions
  '((define x 1)
    (define y #t)
    ;;; etc.
    ))

;;; use: (test-well-formed-things (lambda (v) (and (is-definition? v) (check-toplevel-form v))) well-formed-definitions)
;;; returns #t if all is well

(define well-formed-expressions
  '(42

    #t

    #\a

    "hello world"

    x

    (time 42)

    (if 1 2 3)

    (and)
    (and 1)
    (and 1 2)

    (or)
    (and 1)
    (and 1 2)

    (cond [1 2] [3 4] [else 33])
    (cond [else 33])
    ;;; etc.
    ))

;;; use: (test-well-formed-things check-expression well-formed-expressions)
;;; returns #t if all is well

;;;;;;;;;;

(define the-current-ill-formed-thing
  "not initialized yet")

;;; returns #t if all the ill-formed things are deemed ill formed
;;; returns #f if at least one of the ill-formed things is deemed well formed
;;; in case of hard error, the faulty thing is bound to the-current-ill-formed-thing
(define test-ill-formed-things
  (lambda (check things)
    (letrec ([visit (lambda (things b)
                      (cond
                        [(null? things)
                         (begin
                           (set! the-current-ill-formed-thing "not initialized yet")
                           b)]
                        [(pair? things)
                         (begin
                           (set! the-current-ill-formed-thing (car things))
                           (if (check (car things))
                               (begin
                                 (printf "improperly accepted: ~s~n"
                                         (car things))
                                 (visit (cdr things) #f))
                               (visit (cdr things) b)))]
                        [else
                         (errorf 'test-ill-formed-things
                                 "ill-formed list: ~s"
                                 things)]))])
      (visit things #t))))

(define ill-formed-definitions
  '((define)
    (define . x)
    (define x)
    (define x y . z)
    (define x y z)
    ;;; etc.
    ))

;;; use: (test-ill-formed-things check-definition ill-formed-definitions)
;;; returns #t if all is well

(define ill-formed-expressions
  '(else

    (time)
    (time . 42)
    (time 42 . 43)
    (time 42 43)

    (if)
    (if . 1)
    (if 1)
    (if 1 . 2)
    (if 1 2)
    (if 1 2 . 3)
    (if 1 2 3 . 4)
    (if 1 2 3 4)

    (and . 1)
    (and 1 . 2)
    (and 1 2 . 3)

    (cond)
    (cond . 1)
    (cond else)
    (cond . else)
    (cond [else])
    (cond [esle 33])
    (cond [else . 1] . 2)
    (cond [else 1] 2)
    (cond [] [else 42])
    (cond [1 . 2] [else 42])
    (cond [1 2] [3 4])

    ;;; etc.
    ))

;;; use: (test-ill-formed-things check-expression ill-formed-expressions)
;;; returns #t if all is well

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;

;;;;;

;;; predicate:
(define is-quasiquote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quasiquote))))      

;;; 1st accessor:
(define quasiquote-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
      [(is-number? v)
       (check-number v)]
      [(is-boolean? v)
       (check-boolean v)]
      [(is-character? v)
       (check-character v)]
      [(is-string? v)
       (check-string v)]
      [(is-variable? v)
       (check-variable v)]
      [(is-time? v)
       (check-time-expression (time-1 v))]
      [(is-if? v)
       (check-if-expression (if-1 v) (if-2 v) (if-3 v))]
      [(is-quote? v)
       (check-quote-expression (quote-1 v))]
      [(is-quasiquote? v)
       (check-quasiquote-expression (quasiquote-1 v))]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
      [else
       (begin
         (printf "~s -- unrecognized input: ~s~n"
                 'check-expression
                 v)
         #f)])))

(define check-quasiquote-expression
  (lambda (v)
    (letrec ([visit (lambda (v number-of-nestings)
                      (errorf 'check-quasiquote-expression "not implemented yet"))])
      (visit v 0))))

;;;;;;;;;;

;;; end of week-8-a-syntax-checker-for-Scheme-part-2.scm

"week-8-a-syntax-checker-for-Scheme-part-2.scm"
