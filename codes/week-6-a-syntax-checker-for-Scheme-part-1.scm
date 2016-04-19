;;; week-6-a-syntax-checker-for-Scheme-part-1.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-6-a-syntax-checker-for-Scheme-part-1.html

;;;;;;;;;;

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       (begin
         (printf "check-program -- unrecognized input: ~s~n" v)
         #f)])))

;;;;;;;;;;

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition (define-1 v) (define-2 v))]
      [else
       (check-expression v)])))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;

;;; predicate:
(define is-definition?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'define))))

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (name definiens)
    (and (check-variable name)
         (check-expression definiens))))

;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;

;;;;;

;;; predicate:
(define is-number?
  (lambda (v)
    (number? v)))

;;;;;

;;; predicate:
(define is-boolean?
  (lambda (v)
    (boolean? v)))

;;;;;

;;; predicate:
(define is-character?
  (lambda (v)
    (errorf 'is-character? "not implemented yet")))

;;;;;

;;; predicate:
(define is-string?
  (lambda (v)
    (errorf 'is-string? "not implemented yet")))

;;;;;

;;; predicate:
(define is-variable?
  (lambda (v)
    (errorf 'is-variable? "not implemented yet")))

;;;;;

;;; predicate:
(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'time))))

;;; 1st accessor:
(define time-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

;;; predicate:
(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'if))))

;;; 1st accessor:
(define if-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define if-2
  (lambda (v)
    (list-ref v 2)))

;;; 3rd accessor:
(define if-3
  (lambda (v)
    (list-ref v 3)))

;;;;;

;;; predicate:
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quote))))

;;; 1st accessor:
(define quote-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

;;; predicate:
(define is-application?
  (lambda (v)
    (and (pair? v)
         (let ([w (car v)])
           (if (symbol? w)
               (not (keyword? w))
               #t)))))

;;; 1st accessor:
(define application-operator
  car)

;;; 2nd accessor:
(define application-operands
  cdr)

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
      ;;; cond
      ;;; and
      ;;; or
      ;;; etc.
      [(is-quote? v)
       (check-quote-expression (quote-1 v))]
      [(is-application? v)
       (check-application (application-operator v) (application-operands v))]
      [else
       (begin
         (printf "check-expression -- unrecognized input: ~s~n" v)
         #f)])))

(define check-number
  (lambda (n)
    #t))

(define check-boolean
  (lambda (b)
    #t))

(define check-character
  (lambda (c)
    (errorf 'check-character "not implemented yet")))

(define check-string
  (lambda (s)
    (errorf 'check-string "not implemented yet")))

(define check-variable
  (lambda (v)
    (errorf 'check-variable "not implemented yet")))

(define check-time-expression
  (lambda (v)
    (check-expression v)))

(define check-if-expression
  (lambda (test consequent alternative)
    (and (check-expression test)
         (check-expression consequent)
         (check-expression alternative))))

(define check-quote-expression
  (lambda (v)
    (errorf 'check-quote-expression "not implemented yet")))

(define check-quasiquote-expression
  (lambda (v)
    (letrec ([visit (lambda (v number-of-nestings)
                      (errorf 'check-quasiquote-expression "not implemented yet"))])
      (visit v 0))))

(define check-application
  (lambda (v vs)
    (errorf 'check-application "not implemented yet")))

;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;

(define keyword?
  (lambda (w)
    (errorf 'keyword "not implemented yet")))

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (1- i)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

;;; interface: 
(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

;;;;;;;;;;

;;; Petite Chez Scheme Version 8.4
;;; Copyright (c) 1985-2011 Cadence Research Systems
;;; 
;;; > (load "my-very-own-self-applicable-syntax-checker-oh-yeah.scm")
;;; > (check-file "my-very-own-self-applicable-syntax-checker-oh-yeah.scm")
;;; #t
;;; > (check-file "week-6-basic-imperative-language_a-syntax-checker.scm")
;;; #t
;;; > (check-file "week-6-basic-imperative-language_an-interpreter.scm")
;;; #t
;;; > (check-expression '(if 1 2 3))
;;; #t
;;; > (check-expression '(time is money))
;;; #f
;;; > 

;;;;;;;;;;

;;; end of week-6-a-syntax-checker-for-Scheme-part-1.scm

"week-6-a-syntax-checker-for-Scheme-part-1.scm"
