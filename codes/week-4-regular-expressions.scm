;;; week-4-regular-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 01 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-4-regular-expressions.html

;;;;;;;;;;

;;; utility:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (1- n))))))

;;;;;;;;;;

;;; the constructors:

(define make-empty
  (lambda ()
    (list 'empty)))

(define make-atom
  (lambda (a)
    (list 'atom a)))

(define make-any
  (lambda ()
    (list 'any)))

(define make-seq
  (lambda (re1 re2)
    (list 'seq re1 re2)))

(define make-disj
  (lambda (re1 re2)
    (list 'disj re1 re2)))

(define make-star
  (lambda (re)
    (list 'star re)))

(define make-plus
  (lambda (re)
    (list 'plus re)))

(define make-var
  (lambda (name)
    (list 'var name)))

;;; the predicates:

(define is-empty?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'empty)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-atom?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'atom)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-any?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'any)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-seq?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'seq)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-disj?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'disj)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-star?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'star)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-plus?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'plus)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-var?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'var)
         (proper-list-of-given-length? (cdr v) 1)
         (symbol? (list-ref v 1)))))

;;; the accessors:

(define atom-1
  (lambda (v)
    (list-ref v 1)))

(define seq-1
  (lambda (v)
    (list-ref v 1)))

(define seq-2
  (lambda (v)
    (list-ref v 2)))

(define disj-1
  (lambda (v)
    (list-ref v 1)))

(define disj-2
  (lambda (v)
    (list-ref v 2)))

(define star-1
  (lambda (v)
    (list-ref v 1)))

(define plus-1
  (lambda (v)
    (list-ref v 1)))

(define var-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; sample of well-formed regular expressions:

(define re0
  (make-seq (make-atom 10)
            (make-seq (make-var 'x)
                      (make-atom 30))))

(define re0-alt
  '(seq (atom 10) (seq (var x) (atom 30))))

(define re1
  (make-seq (make-disj (make-var 'x) (make-star (make-any)))
            (make-plus (make-seq (make-var 'z) (make-var 'x)))))

(define re1-alt
  '(seq (disj (var x) (star (any)))
        (plus (seq (var z) (var x)))))

;;; > (equal? re0 re0-alt)
;;; #t
;;; > (equal? re1 re1-alt)
;;; #t
;;; > 

;;; unit test:

(define test-well-formed-regular-expressions
  (lambda (check)
    (and (check re0)
         (check re1)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; syntax checker:

(define check-regular-expression
  (lambda (v)
    (cond
      [(is-empty? v)
       #t]
      [(is-atom? v)
       (number? (atom-1 v))]
      [(is-any? v)
       #t]
      [(is-seq? v)
       (and (check-regular-expression (seq-1 v))
            (check-regular-expression (seq-2 v)))]
      [(is-disj? v)
       (and (check-regular-expression (disj-1 v))
            (check-regular-expression (disj-2 v)))]
      [(is-star? v)
       (check-regular-expression (star-1 v))]
      [(is-plus? v)
       (check-regular-expression (plus-1 v))]
      [(is-var? v)
       (symbol? (var-1 v))]
      [else
       #f])))

;;;  > (test-well-formed-regular-expressions check-regular-expression)
;;;  #t
;;;  > (check-regular-expression "something else entirely")
;;;  #f
;;;  >

;;;;;;;;;;

"week-4-regular-expressions.scm"
