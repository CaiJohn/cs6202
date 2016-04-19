;;; week-6.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 26 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-6.html

;;;;;;;;;;

;;; from week-3-lists-and-defensive-programming.scm:

(define test-append_proper-list
  (lambda (candidate)
    (and (equal? (candidate (list) (list))
                 (list))
         (equal? (candidate (list) (list 4 5 6))
                 (list 4 5 6))
         (equal? (candidate (list 4) (list 5 6))
                 (list 4 5 6))
         (equal? (candidate (list 4 5) (list 6))
                 (list 4 5 6))
         (equal? (candidate (list 4 5 6) (list))
                 (list 4 5 6))
           ;;; add more tests here
         )))

(define append_proper-list
  (lambda (xs_orig ys_orig)
    (letrec ([visit (lambda (xs)
                      (if (null? xs)
                          ;;; base case:
                          ys_orig
                          ;;; induction case:
                          (cons (car xs) (visit (cdr xs)))))])
      (visit xs_orig))))

;;; > (test-append_proper-list append)
;;; #t
;;; > (test-append_proper-list append_proper-list)
;;; #t
;;; > 

;;;;;;;;;;

(define test-reverse
  (lambda (candidate)
    (and (equal? (candidate '())
                 '())
         (equal? (candidate '(1))
                 '(1))
         (equal? (candidate '(1 2))
                 '(2 1))
         (equal? (candidate '(1 2 3))
                 '(3 2 1))
         ;;;
         )))

;;; > (test-reverse reverse)
;;; #t
;;; > 

(define reverse_proper-list-v1
  (lambda (vs_init)
    (letrec ([visit (lambda (vs)
                      (cond
                        [(null? vs)
                         '()]
                        [(pair? vs)
                         (append_proper-list (visit (cdr vs))
                                             (list (car vs)))]
                        [else
                         (errorf 'reverse_proper-list-v1
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (visit vs_init))))

;;; > (test-reverse reverse_proper-list-v1)
;;; #t
;;; > 

;;;;;;;;;;

(define test-rac
  (lambda (candidate)
    (and (equal? (rac '(1 2 3))
                 3)
         (equal? (rac '(1 2))
                 2)
         (equal? (rac '(1))
                 1)
           ;;;
         )))

(define test-rdc
  (lambda (candidate)
    (and (equal? (rdc '(1 2 3))
                 '(1 2))
         (equal? (rdc '(1 2))
                 '(1))
         (equal? (rdc '(1))
                 '())
           ;;;
         )))

(define rac
  (lambda (vs_init)
    (letrec ([visit (lambda (v vs)
                      (cond
                        [(null? vs)
                         v]
                        [(pair? vs)
                         (visit (car vs) (cdr vs))]
                        [else
                         (errorf 'rac
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (if (pair? vs_init)
          (visit (car vs_init) (cdr vs_init))
          (errorf 'rac
                  "not a non-empty list: ~s"
                  vs_init)))))

(define rdc
  (lambda (vs_init)
    (letrec ([visit (lambda (v vs)
                      (cond
                        [(null? vs)
                         '()]
                        [(pair? vs)
                         (cons v (visit (car vs) (cdr vs)))]
                        [else
                         (errorf 'rdc
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (if (pair? vs_init)
          (visit (car vs_init) (cdr vs_init))
          (errorf 'rdc
                  "not a non-empty list: ~s"
                  vs_init)))))

;;; > (test-rac rac)
;;; #t
;;; > (test-rdc rdc)
;;; #t
;;; > 

;;;;;;;;;;

(define reverse_proper-list-v2
  (lambda (vs_init)
    (letrec ([visit (lambda (vs)
                      (cond
                        [(null? vs)
                         '()]
                        [(pair? vs)
                         (cons (rac vs) (visit (rdc vs)))]
                        [else
                         (errorf 'reverse_proper-list-v2
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (visit vs_init))))

;;; > (test-reverse reverse_proper-list-v2)
;;; #t
;;; > 

;;;;;;;;;;

(define reverse_proper-list-v3
  (lambda (vs)
    (letrec ([visit (lambda (i)
                      (if (< i 0)
                          '()
                          (cons (list-ref vs i)
                                (visit (1- i)))))])
      (visit (1- (length vs))))))

;;; > (test-reverse reverse_proper-list-v3)
;;; #t
;;; > 

(define reverse_proper-list-v3_alt1
  (lambda (vs)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          '()
                          (cons (list-ref vs (1- i))
                                (visit (1- i)))))])
      (visit (length vs)))))

(define reverse_proper-list-v3_alt2
  (lambda (vs)
    (letrec ([visit (lambda (i)
                      (cons (list-ref vs i)
                            (if (= i 0)
                                '()
                                (visit (1- i)))))])
      (let ([n (length vs)])
        (if (< n 2)
            vs
            (visit (1- n)))))))

;;;;;;;;;;

(define reverse_proper-list-v4
  (lambda (vs_init)
    (letrec ([visit (lambda (vs a)
                      (cond
                        [(null? vs)
                         a]
                        [(pair? vs)
                         (visit (cdr vs) (cons (car vs) a))]
                        [else
                         (errorf 'reverse_proper-list-v4
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (visit vs_init '()))))

;;; > (test-reverse reverse_proper-list-v4)
;;; #t
;;; > 

;;;;;;;;;;

(define test-mul
  (lambda (candidate)
    (and (equal? (candidate 0 0)
                 0)
         (equal? (candidate 0 5)
                 0)
         (equal? (candidate 5 0)
                 0)
         (equal? (candidate 1 1)
                 1)
         (equal? (candidate 5 1)
                 5)
         (equal? (candidate 1 5)
                 5)
         (equal? (candidate 3 4)
                 12)
         ((lambda (x1 x2)
            (equal? (candidate x1 x2)
                    (* x1 x2)))
          10
          100)
           ;;; add more tests here
         )))

(define mul_acc
  (lambda (n1 n2 a)
    (if (zero? n1)
        a
        (mul_acc (1- n1) n2 (+ n2 a)))))

(define mul_alt-lifted
  (lambda (n1 n2)
    (mul_acc n1 n2 0)))

;;; > (test-mul mul_alt-lifted)
;;; #t
;;; > 

(define mul_alt
  (lambda (n1_orig n2)
    (letrec ([visit (lambda (n1 a)
                      (if (zero? n1)
                          a
                          (visit (1- n1) (+ n2 a))))])
      (visit n1_orig 0))))

;;; > (test-mul mul_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define test-power
  (lambda (candidate)
    (and (equal? (candidate 2 0)
                 1)
         (equal? (candidate 2 1)
                 2)
         (equal? (candidate 2 10)
                 1024)
             ;;; add more tests here
         )))

;;; lambda-lifted version:

(define power_acc
  (lambda (x n a)
    (if (zero? n)
        a
        (power_acc x (1- n) (* x a)))))

(define power_alt-lifted
  (lambda (x n)
    (power_acc x n 1)))

;;; > (test-power power_alt-lifted)
;;; #t
;;; > 

;;; lambda-dropped version:

(define power_alt
  (lambda (x n_orig)
    (letrec ([visit (lambda (n a)
                      (if (zero? n)
                          a
                          (visit (1- n) (* x a))))])
      (visit n_orig 1))))

;;; > (test-power power_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define test-fac
  (lambda (candidate)
    (and (equal? (candidate 0)
                 1)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 5)
                 120)
         (equal? (candidate 7)
                 (* 1 2 3 4 5 6 7))
             ;;; add more tests here
         )))

(define fac_acc
  (lambda (n a)
    (if (zero? n)
        a
        (fac_acc (1- n) (* n a)))))

(define fac_alt-lifted
  (lambda (n)
    (fac_acc n 1)))

;;; > (test-fac fac_alt-lifted)
;;; #t
;;; > 

(define fac_alt
  (lambda (n)
    (letrec ([visit (lambda (n a)
                      (if (zero? n)
                          a
                          (visit (1- n) (* n a))))])
      (visit n 1))))

;;; > (test-fac fac_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define test-sum-from-1-to-max
  (lambda (candidate)
    (and (equal? (candidate 0)
                 (sum-from-1-to-max_constant 0))
         (equal? (candidate 1)
                 (sum-from-1-to-max_constant 1))
         (equal? (candidate 2)
                 (sum-from-1-to-max_constant 2))
         (equal? (candidate 10)
                 (sum-from-1-to-max_constant 10))
           ;;;
         )))

;;;;;;;;;;

(define test-flatten
  (lambda (candidate)
    (and (equal? (candidate '())
                 '())
         (equal? (candidate '(1 2 3))
                 '(1 2 3))
         (equal? (candidate '((1) (2 3)))
                 '(1 2 3))
         (equal? (candidate '(((1) (2)) 3))
                 '(1 2 3))
         (equal? (candidate '(((1) (2)) 3 4))
                 '(1 2 3 4))
         (equal? (candidate '((((() 1) 2) 3) 4))
                 '(() 1 2 3 4))
           ;;;
         )))

;;;;;;;;;;

;;; Exercise 3:

(define compile-arithmetic-expression-using-an-accumulator
  (lambda (e)
    (letrec ([visit (lambda (e a)
                      (cond
                        [(is-literal? e)
                         (cons (make-PUSH (literal-1 e)) a)]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (visit (plus-2 e)
                                       (cons (make-ADD)
                                             a)))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (visit (times-2 e)
                                       (cons (make-MUL)
                                             a)))]
                        [else
                         (errorf 'compile-arithmetic-expression-using-an-accumulator
                                 "unrecognized expression: ~s"
                                 e)]))])
      (make-byte-code-program (visit e '())))))

;;; > (test-compile-arithmetic-expressions compile-arithmetic-expression-using-an-accumulator check-byte-code-program)
;;; #t
;;; > (test-compile-and-run-arithmetic-expressions compile-arithmetic-expression-using-an-accumulator run-byte-code-program)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-6.scm

"week-6.scm"
