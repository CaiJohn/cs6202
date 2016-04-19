;;; week-4-boolean-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 01 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-4-boolean-expressions.html

;;;;;;;;;;

;;; environments:

(define alist-mt
  '())

(define alist-extend
  (lambda (name denotable environment)
    (cons (cons name denotable)
          environment)))

(define alist-lookup
  (lambda (name environment found not-found)
    (letrec ([visit (lambda (e)
                      (if (null? e)
                          (not-found name)
                          (let ([binding (car e)])
                            (if (equal? name (car binding))
                                (found (cdr binding))
                                (visit (cdr e))))))])
      (visit environment))))

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

;;; constructors:

(define make-var
  (lambda (x)
    (list 'var x)))

(define make-conj
  (lambda (f1 f2)
    (list 'conj f1 f2)))

(define make-disj
  (lambda (f1 f2)
    (list 'disj f1 f2)))

(define make-neg
  (lambda (f)
    (list 'neg f)))

;;; predicates:

(define is-var?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'var)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-conj?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'conj)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-disj?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'disj)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-neg?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'neg)
         (proper-list-of-given-length? (cdr v) 1))))

;;; accessors:

(define var-1
  (lambda (v)
    (list-ref v 1)))

(define conj-1
  (lambda (v)
    (list-ref v 1)))

(define conj-2
  (lambda (v)
    (list-ref v 2)))

(define disj-1
  (lambda (v)
    (list-ref v 1)))

(define disj-2
  (lambda (v)
    (list-ref v 2)))

(define neg-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; sample of well-formed Boolean expressions:

(define be0
  (make-conj (make-disj (make-var 'x) (make-neg (make-var 'y)))
             (make-neg (make-conj (make-var 'z) (make-var 'x)))))

(define be0-alt
  '(conj
     (disj (var x) (neg (var y)))
     (neg (conj (var z) (var x)))))

(define be1
  (make-disj (make-var 'x) (make-neg (make-var 'x))))

(define be1-alt
  '(disj (var x) (neg (var x))))

;;; unit test:

(define test-well-formed-boolean-expressions
  (lambda (check)
    (and (check be0)
         (check be1)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; syntax checker:

(define check-boolean-expression
  (lambda (v)
    (cond
      [(is-var? v)
       #t]
      [(is-conj? v)
       (and (check-boolean-expression (conj-1 v))
            (check-boolean-expression (conj-2 v)))]
      [(is-disj? v)
       (and (check-boolean-expression (disj-1 v))
            (check-boolean-expression (disj-2 v)))]
      [(is-neg? v)
       (check-boolean-expression (neg-1 v))]
      [else
       #f])))

;;; > (test-well-formed-boolean-expressions check-boolean-expression)
;;; #t
;;; >

;;;;;;;;;;

(define evaluate-boolean-expression
  (lambda (expression environment)
    (letrec ([visit (lambda (e)
                      (cond
                        [(is-var? e)
                         (alist-lookup (var-1 e)
                                       environment
                                       (lambda (v)
                                         v)
                                       (lambda (x)
                                         (errorf 'evaluate-boolean-expression
                                                 "unbound name: ~s"
                                                 x)))]
                        [(is-conj? e)
                         (let ([v1 (visit (conj-1 e))]
                               [v2 (visit (conj-2 e))])
                           (and v1 v2))]
                        [(is-disj? e)
                         (let ([v1 (visit (disj-1 e))]
                               [v2 (visit (disj-2 e))])
                           (or v1 v2))]
                        [(is-neg? e)
                         (let ([v (visit (neg-1 e))])
                           (not v))]
                        [else
                         (errorf 'evaluate-boolean-expression
                                 "illegal sub-expression: ~s"
                                 e)]))])
      (visit expression))))

;;;;;;;;;;

(define embed-boolean-expression_nnf-into-boolean-expression
  (lambda (v)
    (cond
      [(is-posvar? v)
       (make-var (posvar-1 v))]
      [(is-negvar? v)
       (make-neg (make-var (negvar-1 v)))]
      [(is-conj_nnf? v)
       (make-conj (embed-boolean-expression_nnf-into-boolean-expression
                    (conj_nnf-1 v))
                  (embed-boolean-expression_nnf-into-boolean-expression
                    (conj_nnf-2 v)))]
      [(is-disj_nnf? v)
       (make-disj (embed-boolean-expression_nnf-into-boolean-expression
                    (disj_nnf-1 v))
                  (embed-boolean-expression_nnf-into-boolean-expression
                    (disj_nnf-2 v)))]
      [else
       (errorf 'embed-boolean-expression_nnf-into-boolean-expression
               "not a Boolean expression in negational normal form: ~s"
               v)])))

;;;;;;;;;;

(define normalize-boolean-expression
  (lambda (e)
    (errorf 'normalize-boolean-expression
            "not implemented yet")))

(define evaluate-boolean-expression_nnf
  (lambda (expression environment)
    (errorf 'evaluate-boolean-expression_nnf
            "not implemented yet")))

(define test-the-normalizer
  (lambda (expression environment)
    (equal?
      (evaluate-boolean-expression
        expression
        environment)
      (evaluate-boolean-expression_nnf
        (normalize-boolean-expression expression)
        environment))))

;;;;;;;;;;

"week-4-boolean-expressions.scm"
