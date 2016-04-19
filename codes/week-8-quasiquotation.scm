;;; week-8-quasiquotation.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-8-quasiquotation.html

;;;;;;;;;;

(define fac_Magritte
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (if (= n 0)
                          `1
                          `(* ,n ,(visit (1- n)))))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (visit n_init)
          (errorf 'fac_Magritte
                  "not a non-negative integer: ~s"
                  n_init)))))

;;; > (fac_Magritte 0)
;;; 1
;;; > (fac_Magritte 1)
;;; (* 1 1)
;;; > (fac_Magritte 2)
;;; (* 2 (* 1 1))
;;; > (fac_Magritte 3)
;;; (* 3 (* 2 (* 1 1)))
;;; > (fac_Magritte 4)
;;; (* 4 (* 3 (* 2 (* 1 1))))
;;; > (fac_Magritte 5)
;;; (* 5 (* 4 (* 3 (* 2 (* 1 1)))))
;;; > 

;;;;;;;;;;

(define fac_Magritte-refined
  (lambda (n_init)
    (letrec ([visit (lambda (n)
                      (if (= n 1)  ;;; <---***---
                          `1
                          `(* ,n ,(visit (1- n)))))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (if (= n_init 0)         ;;; <---***---
              `1
              `,(visit n_init))
          (errorf 'fac_Magritte-refined
                  "not a non-negative integer: ~s"
                  n_init)))))

;;; > (fac_Magritte-refined 0)
;;; 1
;;; > (fac_Magritte-refined 1)
;;; 1
;;; > (fac_Magritte-refined 2)
;;; (* 2 1)
;;; > (fac_Magritte-refined 3)
;;; (* 3 (* 2 1))
;;; > (fac_Magritte-refined 5)
;;; (* 5 (* 4 (* 3 (* 2 1))))
;;; > 

;;;;;;;;;;

(define make-representation-of-identity-procedure
  (lambda (x)
    `(lambda (,x)
       ,x)))

;;;;;;;;;;

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

(define try-curried-candidate-transparently
  (lambda (candidate input1 input2 expected-output name)
    (or (equal? ((candidate input1) input2)
                expected-output)
        (begin
          (printf "~s: error for ~s and ~s~n" name input1 input2)
          #f))))

(define test-curried-prepend
  (lambda (candidate)
    (and-all (try-curried-candidate-transparently
              candidate
              '() '()
              '()
              'test-curried-prepend)
             (try-curried-candidate-transparently
              candidate
              '() '(1 2 3 4 5 6)
              '(1 2 3 4 5 6)
              'test-curried-prepend)
             (try-curried-candidate-transparently
              candidate
              '(1) '(2 3 4 5 6)
              '(1 2 3 4 5 6)
              'test-curried-prepend)
             (try-curried-candidate-transparently
              candidate
              '(1 2 3) '(4 5 6)
              '(1 2 3 4 5 6)
              'test-curried-prepend)
             (try-curried-candidate-transparently
              candidate
              '(1 2 3 4 5 6) '()
              '(1 2 3 4 5 6)
              'test-curried-prepend)
             ;;;
             )))

(define curried-prepend
  (lambda (xs_init)
    (lambda (ys_init)
       (letrec ([visit (lambda (xs)
                          (if (null? xs)
                              ys_init
                              (cons (car xs) (visit (cdr xs)))))])
          (visit xs_init)))))

(define curried-prepend_Magritte
  (lambda (xs_init)
    `(lambda (ys_init)
       ,(letrec ([visit (lambda (xs)
                          (if (null? xs)
                              `ys_init
                              `(cons ,(car xs) ,(visit (cdr xs)))))])
          (visit xs_init)))))

;;; > (curried-prepend_Magritte '())
;;; (lambda (ys_init) ys_init)
;;; > (define prepend_
;;;         (lambda (ys_init)
;;;           ys_init))
;;; > (prepend_ '())
;;; ()
;;; > (prepend_ '(1 2 3 4 5 6))
;;; (1 2 3 4 5 6)
;;; > (curried-prepend_Magritte '(1))
;;; (lambda (ys_init) (cons 1 ys_init))
;;; > (define prepend_1
;;;         (lambda (ys_init)
;;;           (cons 1 ys_init)))
;;; > (prepend_1 '())
;;; (1)
;;; > (prepend_1 '(2 3 4 5 6))
;;; (1 2 3 4 5 6)
;;; > (curried-prepend_Magritte '(1 2 3))
;;; (lambda (ys_init) (cons 1 (cons 2 (cons 3 ys_init))))
;;; > (define prepend_123
;;;         (lambda (ys_init)
;;;           (cons 1 (cons 2 (cons 3 ys_init)))))
;;; > (prepend_123 '())
;;; (1 2 3)
;;; > (prepend_123 '(4 5 6))
;;; (1 2 3 4 5 6)
;;; > 

;;;;;;;;;;

(define square
  (lambda (x)
    (* x x)))

(define binary-power
  (lambda (x n)
    (letrec ([visit (lambda (i)
                      (if (even? i)
                          (if (= i 0)
                              1
                              (square (visit (quotient i 2))))
                          (if (= i 1)
                              x
                              (* (square (visit (quotient i 2))) x))))])
      (visit n))))

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

;;; > (test-power binary-power)
;;; #t
;;; > 

(define binary-power_gen
  (lambda (x n)
    `(lambda (,x)
       ,(letrec ([visit (lambda (i)
                          (if (even? i)
                              (if (= i 0)
                                  `1
                                  `(square ,(visit (quotient i 2))))
                              (if (= i 1)
                                  x
                                  `(* (square ,(visit (quotient i 2))) ,x))))])
          (visit n)))))

;;; > (binary-power_gen 'x 0)
;;; (lambda (x) 1)
;;; > (binary-power_gen 'x 1)
;;; (lambda (x) x)
;;; > (binary-power_gen 'x 2)
;;; (lambda (x) (square x))
;;; > (binary-power_gen 'x 3)
;;; (lambda (x) (* (square x) x))
;;; > (binary-power_gen 'x 10)
;;; (lambda (x) (square (* (square (square x)) x)))
;;; > ((lambda (x) (square (* (square (square x)) x))) 2)
;;; 1024
;;; > 

;;;;;;;;;;

;;; Exercise 1

(define try-candidate-transparently
  (lambda (candidate name-of-candidate input expected-output)
    (let ([actual-output (candidate input)])
      (or (equal? actual-output expected-output)
          (begin
            (printf "~s: error for ~s: ~s instead of ~s~n"
                    name-of-candidate
                    input
                    actual-output
                    expected-output)
            #f)))))

(define test-desugar-quasiquote-without-unquote-splicing
  (lambda (candidate . optionals)
    (let ([name-of-candidate (cond
                               [(null? optionals)
                                'test-desugar-quasiquote-without-unquote-splicing]
                               [(null? (cdr optionals))
                                (car optionals)]
                               [else
                                (errorf 'test-desugar-quasiquote-without-unquote-splicing
                                        "too many arguments: ~s"
                                        optionals)])])
      (and-all (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote 42)
                                            '42)
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote #t)
                                            '#t)
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote #\a)
                                            '#\a)
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote "hello world")
                                            '"hello world")
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote x)
                                            '(quote x))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote ())
                                            '(quote ()))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (unquote whatever))
                                            'whatever)
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (unquote (+ 1 2)))
                                            '(+ 1 2))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (42 #t #\a "hello world" x () (unquote whatever) (unquote (+ 1 2))))
                                            '(cons 42 (cons #t (cons #\a (cons "hello world" (cons 'x (cons '() (cons whatever (cons (+ 1 2) '())))))))))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (+ 2 3) 4 . 5))
                                            '(cons 1 (cons (cons '+ (cons 2 (cons 3 '()))) (cons 4 5))))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (unquote (+ 2 3)) 4 . 5))
                                            '(cons 1 (cons (+ 2 3) (cons 4 5))))
               ;;;
               ))))

(define test-desugar-quasiquote-without-unquote-splicing-corner-cases
  (lambda (candidate . optionals)
    (let ([name-of-candidate (cond
                               [(null? optionals)
                                'test-desugar-quasiquote-without-unquote-splicing-corner-cases]
                               [(null? (cdr optionals))
                                (car optionals)]
                               [else
                                (errorf 'test-desugar-quasiquote-without-unquote-splicing-corner-cases
                                        "too many arguments: ~s"
                                        optionals)])])
      (and-all (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote quasiquote)
                                            '(quote quasiquote))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (unquote lambda (+ 2 3)))
                                            '(cons 'unquote (cons 'lambda (cons (cons '+ (cons 2 (cons 3 '()))) '()))))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (lambda unquote (+ 2 3)))
                                            '(cons 'lambda (+ 2 3)))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (unquote unquote (+ 2 3)))
                                            '(cons 'unquote (+ 2 3)))
               ;;;
               ))))

(define desugar-quasiquote-without-unquote-splicing
  (lambda (v)
    (cond
      [(not (and (pair? v)
                 (equal? (car v) 'quasiquote)
                 (pair? (cdr v))
                 (null? (cdr (cdr v)))))
       (begin
         (printf "desugar-quasiquote -- invalid input~s~n" v)
         #f)]
      [else
       (errorf 'desugar-quasiquote
               "not implemented yet")])))

;;;;;;;;;;

;;; Exercise 2

(define test-desugar-quasiquote-with-unquote-splicing
  (lambda (candidate . optionals)
    (let ([name-of-candidate (cond
                               [(null? optionals)
                                'test-desugar-quasiquote-with-unquote-splicing]
                               [(null? (cdr optionals))
                                (car optionals)]
                               [else
                                (errorf 'test-desugar-quasiquote-with-unquote-splicing
                                        "too many arguments: ~s"
                                        optionals)])])
      (and-all (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (unquote-splicing x) 4 5))
                                            '(cons 1 (append x (cons 4 (cons 5 '())))))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (unquote-splicing (append '(10 20 30) '(40 50 60))) 4 . 5))
                                            '(cons 1 (append (append '(10 20 30) '(40 50 60)) (cons 4 5))))
               ;;;
               ))))

(define desugar-quasiquote-with-unquote-splicing
  (lambda (v)
    (cond
      [(not (and (pair? v)
                 (equal? (car v) 'quasiquote)
                 (pair? (cdr v))
                 (null? (cdr (cdr v)))))
       (begin
         (printf "desugar-quasiquote -- invalid input~s~n" v)
         #f)]
      [else
       (errorf 'desugar-quasiquote
               "not implemented yet")])))

;;;;;;;;;;

;;; Exercise 3

(define test-desugar-quasiquote-with-list-rather-than-cons
  (lambda (candidate . optionals)
    (let ([name-of-candidate (cond
                               [(null? optionals)
                                'test-desugar-quasiquote-with-list-rather-than-cons]
                               [(null? (cdr optionals))
                                (car optionals)]
                               [else
                                (errorf 'test-desugar-quasiquote-with-list-rather-than-cons
                                        "too many arguments: ~s"
                                        optionals)])])
      (and-all (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (42 #t #\a "hello world" x () (unquote whatever) (unquote (+ 1 2))))
                                            '(list 42 #t #\a "hello world" 'x '() whatever (+ 1 2)))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (unquote (+ 2 3)) 4 . 5))
                                            '(cons 1 (cons (+ 2 3) (cons 4 5))))
               (try-candidate-transparently candidate
                                            name-of-candidate
                                            '(quasiquote (1 (unquote-splicing x) 4 5))
                                            '(cons 1 (append x (list 4 5))))
               ;;;
               ))))

(define desugar-quasiquote-with-list-rather-than-cons
  (lambda (v)
    (cond
      [(not (and (pair? v)
                 (equal? (car v) 'quasiquote)
                 (pair? (cdr v))
                 (null? (cdr (cdr v)))))
       (begin
         (printf "desugar-quasiquote -- invalid input~s~n" v)
         #f)]
      [else
       (errorf 'desugar-quasiquote
               "not implemented yet")])))

;;;;;;;;;;

;;; end of week-8-quasiquotation.scm

"week-8-quasiquotation.scm"
