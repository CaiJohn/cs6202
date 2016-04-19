;;; week-8.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-8.html

;;;;;;;;;;

(define try-candidate-transparently
  (lambda (candidate input expected-output name)
    (or (equal? (candidate input)
                expected-output)
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define and-all
    (lambda bs_init
      (letrec ([visit (lambda (bs)
                        (or (null? bs)
                            (and (car bs)
                                 (visit (cdr bs)))))])
        (visit bs_init))))

;;;;;;;;;;

(define test-plus1
  (lambda (candidate)
    (and-all (try-candidate-transparently candidate 0 1 'test-plus1)
             (try-candidate-transparently candidate -1 0 'test-plus1)
             (try-candidate-transparently candidate 7 8 'test-plus1)
             (try-candidate-transparently candidate -8 -7 'test-plus1)
             ;;;
             )))

(define plus1
  (lambda (n)
    (+ 1 n)))

(define plus2
  (lambda (n)
    (+ 2 n)))

(define plus3
  (lambda (n)
    (+ 3 n)))

;;;;;;;;;;

(define adder
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus1_alt
  (adder 1))

(define plus2_alt
  (adder 2))

(define plus3_alt
  (adder 3))

;;; > (test-plus1 plus1)
;;; #t
;;; > (test-plus1 plus1_alt)
;;; #t
;;; > 

(define adder
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define muller
  (lambda (m)
    (lambda (n)
      (* m n))))

(define super-duper
  (lambda (p)
    (lambda (m)
      (lambda (n)
        (p m n)))))

(define adder_alt
  (super-duper +))

(define muller_alt
  (super-duper *))

;;; > (test-plus1 ((super-duper +) 1))
;;; 11
;;; > 

;;;;;;;;;;

(define curry
  (lambda (p)
    (lambda (x1)
      (lambda (x2)
        (p x1 x2)))))

(define uncurry
  (lambda (p)
    (lambda (x1 x2)
      ((p x1) x2))))

(define Curry
  (lambda (n p)
    (errorf 'Curry "not implemented yet")))

(define curried-Curry
  (lambda (n)
    (lambda (p)
      (errorf 'curried-Curry "not implemented yet"))))

(define curry_alt
  (curried-Curry 2))

(define curry3_alt
  (curried-Curry 3))

;;;;;;;;;;

;;; the single "how"'s:
(define list-ref-1
  (lambda (v)
    (list-ref v 1)))

(define list-ref-2
  (lambda (v)
    (list-ref v 2)))

;;; the generic "how":
(define make-list-ref
  (lambda (i)
    (lambda (v)
      (list-ref v i))))

;;; the single "how"'s, revisited:
(define list-ref-1_alt
  (make-list-ref 1))

(define list-ref-2_alt
  (make-list-ref 2))

;;;;;;;;;;

(define plus
  (lambda (m n)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          n
                          (1+ (visit (1- i)))))])
      (visit m))))

(define times
  (lambda (m n)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          0
                          (+ n (visit (1- i)))))])
      (visit m))))

;;;;;;;;;;

(define fold-right_nat
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (lambda (i)
                  (if (= i 0)
                      zero-case
                      (succ-case (visit (1- i)))))])
        (visit n)))))

(define fold-right_nat-traced
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (trace-lambda visit_fold-right_nat (i)
                  (if (= i 0)
                      zero-case
                      (succ-case (visit (1- i)))))])
        (visit n)))))

;;;;;;;;;;

;;; from a previous week:

(define test-plus
  (lambda (candidate)
    (and (equal? (candidate 0 0)
                 0)
         (equal? (candidate 0 5)
                 5)
         (equal? (candidate 5 0)
                 5)
         (equal? (candidate 3 4)
                 7)
         (equal? (candidate 123 4321)
                 4444)
         ((lambda (x1 x2)
            (equal? (candidate x1 x2)
                    (+ x1 x2)))
          10
          100)
         ;;; add more tests here
         )))

(define plus_alt
  (lambda (m n)
    ((fold-right_nat n
                     (lambda (c)
                       (1+ c)))
     m)))

;;; from a previous week:

(define test-times
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

(define times_alt
  (lambda (m n)
    ((fold-right_nat 0
                     (lambda (c)
                       (+ n c)))
     m)))

(define times_alt-gen
  (lambda (m n)
    `(lambda (,n)
       ,((fold-right_nat 0
                         (lambda (c)
                           `(+ ,n ,c)))
         m))))

;;;;;;;;;;

;;; Exercise 3

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

;;;;;;;;;;

(define plus-acc
  (lambda (m n)
    (letrec ([visit (lambda (i a)
                      (if (zero? i)
                          a
                          (visit (1- i) (1+ a))))])
      (visit m n))))

(define times-acc
  (lambda (m n)
    (letrec ([visit (lambda (i a)
                      (if (zero? i)
                          a
                          (visit (1- i) (+ n a))))])
      (visit m 0))))

;;; > (test-plus plus-acc)
;;; #t
;;; > (test-times times-acc)
;;; #t
;;; > 

(define fold-left_nat
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (lambda (i a)
                  (if (= i 0)
                      a
                      (visit (1- i) (succ-case a))))])
        (visit n zero-case)))))

(define fold-left_nat-traced
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (trace-lambda visit_fold-left_nat (i a)
                  (if (= i 0)
                      a
                      (visit (1- i) (succ-case a))))])
        (visit n zero-case)))))

(define plus-acc_alt
  (lambda (m n)
    ((fold-left_nat n
                    (lambda (c)
                      (1+ c)))
     m)))

(define times-acc_alt
  (lambda (m n)
    ((fold-left_nat 0
                    (lambda (c)
                      (+ n c)))
     m)))

;;; > (test-add plus-acc_alt)
;;; #t
;;; > (test-times times-acc_alt)
;;; #t
;;; > 

(define times-acc_alt-gen
    (lambda (m n)
      `(lambda (,n)
         ,((fold-left_nat 0
                          (lambda (c)
                            `(+ ,n ,c)))
           m))))

;;;;;;;;;;

(define fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (if (null? ws)
                            nil-case
                            (cons-case (car ws)
                                       (visit (cdr ws)))))])
        (visit vs)))))

(define fold-left_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws a)
                        (if (null? ws)
                            a
                            (visit (cdr ws) (cons-case (car ws) a))))])
        (visit vs nil-case)))))

(define test-length_proper-list
  (lambda (candidate)
    (and (equal? (candidate (list))
                 0)
         (equal? (candidate (list 1 2 3))
                 3)
         ;;; add more tests here
         )))

(define length_proper-list_alt
  (lambda (xs)
    ((fold-right_proper-list 0
                             (lambda (x c)
                               (1+ c)))
     xs)))

(define length-acc_proper-list_alt
  (lambda (xs)
    ((fold-left_proper-list 0
                            (lambda (x c)
                              (1+ c)))
     xs)))

;;; > (test-length_proper-list length_proper-list_alt)
;;; #t
;;; > (test-length_proper-list length-acc_proper-list_alt)
;;; #t
;;; > 

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

(define append_proper-list_alt
  (lambda (xs ys)
    ((fold-right_proper-list ys
                             (lambda (x c)
                               (cons x c)))
     xs)))

;;; > (test-append_proper-list append_proper-list_alt)
;;; #t
;;; > 

(define append_proper-list_alt-gen
  (lambda (xs ys)
    `(lambda (,ys)
       ,((fold-right_proper-list ys
                                 (lambda (x c)
                                   `(cons ',x ,c)))
         xs))))

;;;;;;;;;;

(define test-plus-variadic
  (lambda (candidate)
    (and (equal? (candidate)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 1 10)
                 11)
         (equal? (candidate 1 10 100)
                 111)
         (equal? (candidate 1 10 100 1000)
                 1111)
           ;;;
         )))

(define plus-dyadic
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1)
                      (if (= n1 0)
                          n2
                          (1+ (visit (1- n1)))))])
      (if (and (integer? n1)
               (>= n1 0))
          (visit n1)
          (errorf 'plus-dyadic "not a non-negative number: ~s" n1)))))

(define plus-variadic_alt
  (lambda ns
    ((fold-right_proper-list 0 plus-dyadic) ns)))

;;; > (test-plus-variadic plus-variadic_alt)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 8

(define putative-fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (((fold-left_proper-list (lambda (a)
                                 a)
                               (lambda (x h)
                                 (lambda (a)
                                   (h (cons-case x a)))))
        vs)
       nil-case))))

(define putative-fold-left_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (((fold-right_proper-list (lambda (a)
                                  a)
                                (lambda (x h)
                                  (lambda (a)
                                    (h (cons-case x a)))))
        vs)
       nil-case))))

;;;;;;;;;;

;;; end of week-8.scm

"week-8.scm"
