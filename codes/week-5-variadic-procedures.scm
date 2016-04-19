;;; week-5-variadic-procedures.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 10 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-5-variadic-procedures.html

;;;;;;;;;;

(define mystery-procedure
  (lambda xs
    (reverse xs)))

;;;;;;;;;;

;;; Exercise 2a:

(define test-variadic-palindrome?
  (lambda (candidate)
    (and (equal? (candidate)
                 #t)
         (equal? (candidate 1)
                 #t)
         (equal? (candidate 1 1)
                 #t)
         (equal? (candidate 1 2 1)
                 #t)
         (equal? (candidate 1 2 2 1)
                 #t)
         (equal? (candidate 1 2 3 2 1)
                 #t)
         (equal? (candidate 1 2 3 3 2 1)
                 #t)
         ;;;
         (equal? (candidate 1 2)
                 #f)
         (equal? (candidate 1 2 1 2)
                 #f)
         (equal? (candidate 1 2 1 2 4 5 2 1 2 1)
                 #f)
         ;;;
         )))

(define variadic-palindrome?
  (lambda xs
    (errorf 'variadic-palindrome? "not implemented yet")))

;;; > (test-variadic-palindrome? variadic-palindrome?)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 2b:

(define test-the-odd-arguments
  (lambda (candidate)
    (and (equal? (candidate)
                 '())
         (equal? (candidate 1)
                 '(1))
         (equal? (candidate 1 2)
                 '(1))
         (equal? (candidate 1 2 3)
                 '(1 3))
         (equal? (candidate 1 2 3 4)
                 '(1 3))
         (equal? (candidate 1 2 3 4 5)
                 '(1 3 5))
         (equal? (candidate 1 2 3 4 5 6 7 8 9 10)
                 '(1 3 5 7 9))
              ;;;
         )))

;;; lambda-dropped version:

(define the-odd-arguments_ld
  (lambda xs
    (errorf 'the-odd-arguments_ld "not implemented yet")))

;;; > (test-the-odd-arguments the-odd-arguments_ld)
;;; #t
;;; > 

;;; lambda-lifted version:

(define the-odd-arguments_ll
  (lambda xs
    (errorf 'the-odd-arguments_ll "not implemented yet")))

;;; > (test-the-odd-arguments the-odd-arguments_ll)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 2c:

(define test-the-even-arguments-in-opposite-order
  (lambda (candidate)
    (and (equal? (candidate)
                 '())
         (equal? (candidate 1)
                 '())
         (equal? (candidate 1 2)
                 '(2))
         (equal? (candidate 1 2 3)
                 '(2))
         (equal? (candidate 1 2 3 4)
                 '(4 2))
         (equal? (candidate 1 2 3 4 5)
                 '(4 2))
         (equal? (candidate 1 2 3 4 5 6 7 8 9 10)
                 '(10 8 6 4 2))
              ;;;
         )))

(define the-even-arguments-in-opposite-order_ld
  (lambda xs
    (errorf 'the-even-arguments-in-opposite-order_ld "not implemented yet")))

;;; > (test-the-even-arguments-in-opposite-order the-even-arguments-in-opposite-order_ld)
;;; #t
;;; > 

(define the-even-arguments-in-opposite-order_ll
  (lambda xs
    (errorf 'the-even-arguments-in-opposite-order_ll "not implemented yet")))

;;; > (test-the-even-arguments-in-opposite-order the-even-arguments-in-opposite-order_ll)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 2d:

(define test-self-convolve
  (lambda (candidate)
    (and (equal? (candidate)
                 '())
         (equal? (candidate 1)
                 '((1 . 1)))
         (equal? (candidate 1 2)
                 '((1 . 2) (2 . 1)))
         (equal? (candidate 1 2 3)
                 '((1 . 3) (2 . 2) (3 . 1)))
         (equal? (candidate 1 2 3 4)
                 '((1 . 4) (2 . 3) (3 . 2) (4 . 1)))
         (equal? (candidate 1 2 3 4 5)
                 '((1 . 5) (2 . 4) (3 . 3) (4 . 2) (5 . 1)))
         (equal? (candidate 1 2 3 4 5 6 7 8 9 10)
                 '((1 . 10) (2 . 9) (3 . 8) (4 . 7) (5 . 6) (6 . 5) (7 . 4) (8 . 3) (9 . 2) (10 . 1)))
         ;;;
         )))

(define self-convolve_ld
  (lambda xs
    (errorf 'self-convolve_ld "not implemented yet")))
      
;;; > (test-self-convolve self-convolve_ld)
;;; #t
;;; > 

(define self-convolve_ll
  (lambda xs
    (errorf 'self-convolve_ll "not implemented yet")))

;;; > (test-self-convolve self-convolve_ll)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 2e:

(define test-convolve-the-odd-arguments-and-the-even-arguments
  (lambda (candidate)
    (and (equal? (candidate)
                 '())
         (equal? (candidate 1)
                 '())
         (equal? (candidate 1 2)
                 '((1 . 2)))
         (equal? (candidate 1 2 3)
                 '((1 . 2)))
         (equal? (candidate 1 2 3 4)
                 '((1 . 4) (3 . 2)))
         (equal? (candidate 1 2 3 4 5)
                 '((1 . 4) (3 . 2)))
         (equal? (candidate 1 2 3 4 5 6 7 8 9 10)
                 '((1 . 10) (3 . 8) (5 . 6) (7 . 4) (9 . 2)))
         ;;;
         )))

(define convolve-the-odd-arguments-and-the-even-arguments
  (lambda xs
    (errorf 'convolve-the-odd-arguments-and-the-even-arguments "not implemented yet")))

;;;;;;;;;;

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

(define plus-dyadic
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1)
                      (if (= n1 0)
                          n2
                          (1+ (visit (1- n1)))))])
      (if (and (integer? n1)
               (>= n1 0))
          (visit n1)
          (errorf 'plus-dyadic "not a non-negative integer: ~s" n1)))))

;;; > (test-plus plus-dyadic)
;;; #t
;;; > 

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

(define plus-variadic
  (lambda ns
    (letrec ([visit (lambda (ns)
                      (if (null? ns)
                          0
                          (plus-dyadic (car ns)
                                       (visit (cdr ns)))))])
      (visit ns))))

;;; > (test-plus plus-variadic)
;;; #t
;;; > (test-plus-variadic plus-variadic)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-5-variadic-procedures.scm

"week-5-variadic-procedures.scm"
