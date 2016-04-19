;;; week-2.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 27 Aug 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-2.html

;;;;;;;;;;

(define not-as-a-user-defined-procedure
  (lambda (v)
    (if v
        #f
        #t)))

;;; > (not #t)
;;; #f
;;; > (not #f)
;;; #t
;;; > (not 42)
;;; #f
;;; > 
;;; > (not-as-a-user-defined-procedure #t)
;;; #f
;;; > (not-as-a-user-defined-procedure #f)
;;; #t
;;; > (not-as-a-user-defined-procedure 42)
;;; #f
;;; > 

;;;;;;;;;;

(define not-not
  (lambda (v)
    (not (not v))))

;;;;;;;;;;

(define not-not-not
  (lambda (v)
    (not (not (not v)))))

;;;;;;;;;;

(define plus
  (lambda (n1 n2)
    (if (zero? n1)
        ; the base case:
        n2
        ; the induction case:
        (1+ (plus (1- n1) n2)))))

;;; > (plus 2 4)
;;; 6
;;; > 

;;;;;;;;;;

(define plus_traced
  (trace-lambda plus (n1 n2)
    (if (zero? n1)
        n2
        (1+ (plus_traced (1- n1) n2)))))

;;; > (plus_traced 2 4)
;;; |(plus 2 4)
;;; | (plus 1 4)
;;; | |(plus 0 4)
;;; | |4
;;; | 5
;;; |6
;;; 6
;;; > 

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

;;; > (test-plus plus)
;;; #t
;;; > 

;;;;;;;;;;

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

;;;;;;;;;;

(define times
  (lambda (n1 n2)
    (if (zero? n1)
        0
        (plus n2 (times (1- n1) n2)))))

;;; > (test-times times)
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

(define power
  (lambda (x n)
    (if (zero? n)
        1
        (times x (power x (1- n))))))

;;; > (test-power power)
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

(define fac
  (lambda (n)
    (if (zero? n)
        1
        (times n (fac (1- n))))))

;;; > (test-fac fac)
;;; #t
;;; > 

;;;;;;;;;;

(define fac_traced
  (trace-lambda fac (n)
    (if (= n 0)
        1
        (times n (fac_traced (1- n))))))

;;; > (fac_traced 5)
;;; |(fac 5)
;;; | (fac 4)
;;; | |(fac 3)
;;; | | (fac 2)
;;; | | |(fac 1)
;;; | | | (fac 0)
;;; | | | 1
;;; | | |1
;;; | | 2
;;; | |6
;;; | 24
;;; |120
;;; 120
;;; > 

;;; Question: what happens if you evaluate
;;;   (test-fac fac_traced)
;;; ?

;;;;;;;;;;

(define plus_alt
  (lambda (n1 n2)
    (if (zero? n1)
        ; the base case:
        n2
        ; the induction case:
        (plus_alt (1- n1) (1+ n2)))))

;;; > (test-plus plus_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define plus_alt_traced
  (trace-lambda plus_alt (n1 n2)
    (if (zero? n1)
        n2
        (plus_alt_traced (1- n1) (1+ n2)))))

;;; > (plus_alt_traced 2 4)
;;; |(plus_alt 2 4)
;;; |(plus_alt 1 5)
;;; |(plus_alt 0 6)
;;; |6
;;; 6
;;; > 

;;;;;;;;;;

(define is-even?
  (lambda (n)
    (if (= n 0)
        #t
        (is-odd? (1- n)))))

(define is-odd?
  (lambda (n)
    (if (= n 0)
        #f
        (is-even? (1- n)))))

(define test-is-even?
  (lambda (candidate)
    (and (equal? (candidate 0)
                 #t)
         (equal? (candidate 1)
                 #f)
         (equal? (candidate 2)
                 #t)
         (equal? (candidate 3)
                 #f)
         (equal? (candidate 4)
                 #t)
         (equal? (candidate 100)
                 #t)
         (equal? (candidate 101)
                 #f)
             ;;; 
         )))

(define test-is-odd?
  (lambda (candidate)
    (and (equal? (candidate 0)
                 #f)
         (equal? (candidate 1)
                 #t)
         (equal? (candidate 2)
                 #f)
         (equal? (candidate 3)
                 #t)
         (equal? (candidate 4)
                 #f)
         (equal? (candidate 100)
                 #f)
         (equal? (candidate 101)
                 #t)
             ;;;
         )))

;;; Evaluate
;;;  (test-is-even? is-even?)
;;; and
;;;  (test-is-odd? is-odd?)

(define is-even?_alt
  (lambda (n)
    (= (remainder n 2) 0)))

(define is-odd?_alt
  (lambda (n)
    (= (remainder n 2) 1)))

;;; > (test-is-even? is-even?_alt)
;;; #t
;;; > (test-is-odd? is-odd?_alt)
;;; #t
;;; > (test-is-even? even?)
;;; #t
;;; > (test-is-odd? odd?)
;;; #t
;;; > 

;;;;;;;;;;

(define test-number-of-trailing-zeroes-in-a-positive-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 42)
                 0)
         (equal? (candidate 10)
                 1)
         (equal? (candidate 500)
                 2)
           ;;; add more tests here
         )))

(define number-of-trailing-zeroes-in-a-positive-integer-in-base-10
  (lambda (n)
    (if (= (remainder n 10) 0)
        (if (= n 0)
            0
            (1+ (number-of-trailing-zeroes-in-a-positive-integer-in-base-10 (quotient n 10))))
        0)))

;;; > (test-number-of-trailing-zeroes-in-a-positive-integer-in-base-10
;;;    number-of-trailing-zeroes-in-a-positive-integer-in-base-10)
;;; #t
;;; > 

;;;;;;;;;;

(define test-number-of-significant-zero-digits-in-a-positive-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 10)
                 1)
         (equal? (candidate 100)
                 2)
         (equal? (candidate 1010)
                 2)
         (equal? (candidate 00100100)
                 4)
           ;;; add more tests here
         )))

(define number-of-significant-zero-digits-in-a-positive-integer-in-base-10
  (lambda (n)
    (if (= (remainder n 10) 0)
        (if (= n 0)
            0
            (1+ (number-of-significant-zero-digits-in-a-positive-integer-in-base-10 (quotient n 10))))
        (number-of-significant-zero-digits-in-a-positive-integer-in-base-10 (quotient n 10)))))

;;; > (test-number-of-significant-zero-digits-in-a-positive-integer-in-base-10
;;;         number-of-significant-zero-digits-in-a-positive-integer-in-base-10)
;;; #t
;;; > 

;;;;;;;;;;

(define test-number-of-significant-digits-in-a-non-negative-integer-in-base-8
  (lambda (candidate)
    (and (equal? (candidate (+ (* 7 (expt 8 0))))
                 1)  ;;; 7 in base 8
         (equal? (candidate (+ (* 7 (expt 8 1))
                               (* 7 (expt 8 0))))
                 2)  ;;; 77 in base 8
         (equal? (candidate (+ (* 7 (expt 8 2))
                               (* 7 (expt 8 1))
                               (* 7 (expt 8 0))))
                 3)  ;;; 777 in base 8
         (equal? (candidate (+ (* 7 (expt 8 3))
                               (* 7 (expt 8 2))
                               (* 7 (expt 8 1))
                               (* 7 (expt 8 0))))
                 4)  ;;; 7777 in base 8
         (equal? (candidate (+ (* 7 (expt 8 4))
                               (* 7 (expt 8 3))
                               (* 7 (expt 8 2))
                               (* 7 (expt 8 1))
                               (* 7 (expt 8 0))))
                 5)  ;;; 77777 in base 8
             ;;; add more tests here
         )))

;;;;;;;;;;

(define test-number-of-one-digits-in-a-non-negative-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 5)
                 0)
         (equal? (candidate 10)
                 1)
         (equal? (candidate 11)
                 2)
         (equal? (candidate 012340123401234)
                 3)
             ;;; add more tests here
         )))

(define test-number-of-two-digits-in-a-non-negative-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 5)
                 0)
         (equal? (candidate 20)
                 1)
         (equal? (candidate 22)
                 2)
         (equal? (candidate 012340123401234)
                 3)
             ;;; add more tests here
         )))

(define test-number-of-three-digits-in-a-non-negative-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 5)
                 0)
         (equal? (candidate 30)
                 1)
         (equal? (candidate 33)
                 2)
         (equal? (candidate 012340123401234)
                 3)
             ;;; add more tests here
         )))

(define test-number-of-given-digit-in-a-non-negative-integer-in-base-10
  (lambda (candidate)
    (and (equal? (candidate 5 43210)
                 0)
         (equal? (candidate 9 99)
                 2)
         (equal? (candidate 8 012345678901234567890123456789)
                 3)
             ;;; add more tests here
         )))

;;;;;;;;;;

;;; Version with a linear number of arithmetic operations:
(define sum-from-1-to-max_linear
  (lambda (max)
    (if (= max 0)
        0
        (+ max (sum-from-1-to-max_linear (1- max))))))

;;; Version with a constant number of arithmetic operations:
(define sum-from-1-to-max_constant
  (lambda (max)
    (/ (* max (+ max 1)) 2)))

;;;;;;;;;;

(define test-sum-from-1-to-max
  (lambda (n)
    (= (sum-from-1-to-max_linear n)
       (sum-from-1-to-max_constant n))))

;;; > (test-sum-from-1-to-max 100)
;;; #t
;;; > 

;;;;;;;;;;

(define test-fib
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 2)
                 1)
         (equal? (candidate 3)
                 2)
         (equal? (candidate 4)
                 3)
         (equal? (candidate 5)
                 5)
         (equal? (candidate 6)
                 8)
         (equal? (candidate 7)
                 13)
         (equal? (candidate 8)
                 21)
         (equal? (candidate 9)
                 34)
           ;;; add more tests, based on the Fibonacci sequence
         )))

;;;;;;;;;;

(define test-fib3
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 2)
                 2)
         (equal? (candidate 3)
                 (+ 0
                    1
                    2))
         (equal? (candidate 4)
                 (+ 1
                    2
                    (+ 0 1 2)))
         (equal? (candidate 5)
                 (+ 2
                    (+ 0 1 2)
                    (+ 1 2 (+ 0 1 2))))
         (equal? (candidate 6)
                 (+ (+ 0 1 2)
                    (+ 1 2 (+ 0 1 2))
                    (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2)))))
         (equal? (candidate 7)
                 (+ (+ 1 2 (+ 0 1 2))
                    (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2)))
                    (+ (+ 0 1 2) (+ 1 2 (+ 0 1 2)) (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2))))))
         ;;; add more tests
         )))

;;;;;;;;;;

(define cube
  (lambda (n)
    (* n n n)))

;;; Version with a linear number of arithmetic operations:
(define sum-of-the-first-cubes_linear
  (lambda (max)
    (if (= max 0)
        0
        (+ (cube max) (sum-of-the-first-cubes_linear (1- max))))))

(define square
  (lambda (n)
    (* n n)))

;;; Other version with a linear number of arithmetic operations:
(define sum-of-the-first-cubes_linear_alt
  (lambda (max)
    (square (sum-from-1-to-max_linear max))))

;;; Version with a constant number of arithmetic operations:
(define sum-of-the-first-cubes_constant
  (lambda (max)
    (square (sum-from-1-to-max_constant max))))

;;;;;;;;;;

(define test-sum-of-the-first-cubes
  (lambda (n)
    (= (sum-of-the-first-cubes_linear n)
       (sum-of-the-first-cubes_linear_alt n)
       (sum-of-the-first-cubes_constant n))))

;;; > (test-sum-of-the-first-cubes 0)
;;; #t
;;; > (test-sum-of-the-first-cubes 1)
;;; #t
;;; > (test-sum-of-the-first-cubes 10)
;;; #t
;;; > 

;;;;;;;;;;

(define test-sum-of-the-first-cubes_timed
  (lambda (n)
    (= (time (sum-of-the-first-cubes_linear_alt n))
       (time (sum-of-the-first-cubes_constant n))
           (time (sum-of-the-first-cubes_linear n)))))

;;; > (test-sum-of-the-first-cubes_timed 10000)
;;; (time (sum-of-the-first-cubes_constant n))
;;;     no collections
;;;     0 ms elapsed cpu time
;;;     0 ms elapsed real time
;;;     16 bytes allocated
;;; (time (sum-of-the-first-cubes_linear n))
;;;     no collections
;;;     8 ms elapsed cpu time
;;;     7 ms elapsed real time
;;;     376136 bytes allocated
;;; (time (sum-of-the-first-cubes_linear_alt n))
;;;     no collections
;;;     2 ms elapsed cpu time
;;;     3 ms elapsed real time
;;;     240 bytes allocated
;;; #t
;;; > (test-sum-of-the-first-cubes_timed 100000)
;;; (time (sum-of-the-first-cubes_constant n))
;;;     no collections
;;;     0 ms elapsed cpu time
;;;     0 ms elapsed real time
;;;     48 bytes allocated
;;; (time (sum-of-the-first-cubes_linear n))
;;;     2 collections
;;;     61 ms elapsed cpu time, including 5 ms collecting
;;;     65 ms elapsed real time, including 6 ms collecting
;;;     6015496 bytes allocated, including 10426536 bytes reclaimed
;;; (time (sum-of-the-first-cubes_linear_alt n))
;;;     no collections
;;;     19 ms elapsed cpu time
;;;     18 ms elapsed real time
;;;     2302328 bytes allocated
;;; #t
;;; > (test-sum-of-the-first-cubes_timed 1000000)
;;; (time (sum-of-the-first-cubes_constant n))
;;;     no collections
;;;     0 ms elapsed cpu time
;;;     0 ms elapsed real time
;;;     48 bytes allocated
;;; (time (sum-of-the-first-cubes_linear n))
;;;     19 collections
;;;     619 ms elapsed cpu time, including 124 ms collecting
;;;     639 ms elapsed real time, including 131 ms collecting
;;;     77274504 bytes allocated, including 74851192 bytes reclaimed
;;; (time (sum-of-the-first-cubes_linear_alt n))
;;;     9 collections
;;;     302 ms elapsed cpu time, including 95 ms collecting
;;;     306 ms elapsed real time, including 98 ms collecting
;;;     38968992 bytes allocated, including 18024824 bytes reclaimed
;;; #t
;;; > 

;;;;;;;;;;

(define test-odd-number-of-ones?
  (lambda (candidate)
    (and (equal? (candidate "1")
                 "yes")
         (equal? (candidate "001000")
                 "yes")
         (equal? (candidate "00111000")
                 "yes")
         (equal? (candidate "0010101000")
                 "yes")
         (equal? (candidate "")
                 "stuck")
         (equal? (candidate "0")
                 "stuck")
         (equal? (candidate "11")
                 "stuck")
         (equal? (candidate "00101010001")
                 "stuck")
         (equal? (candidate "001010100010")
                 "stuck")
         (equal? (candidate "xy")
                 "error")
         (equal? (candidate "0xy1")
                 "error")
         (equal? (candidate "1xy0")
                 "error")
           ;;; add more tests here
         )))

(define state-with-even-number-of-ones
  (lambda (s i max)
    (if (= i max)
        "stuck"
        (if (char=? (string-ref s i) #\0)
            (state-with-even-number-of-ones s (1+ i) max)
            (if (char=? (string-ref s i) #\1)
                (state-with-odd-number-of-ones s (1+ i) max)
                "error")))))

(define state-with-odd-number-of-ones
  (lambda (s i max)
    (if (= i max)
        "yes"
        (if (char=? (string-ref s i) #\0)
            (state-with-odd-number-of-ones s (1+ i) max)
            (if (char=? (string-ref s i) #\1)
                (state-with-even-number-of-ones s (1+ i) max)
                "error")))))

(define odd-number-of-ones?
  (lambda (s)
    (state-with-even-number-of-ones s 0 (string-length s))))

;;; > (test-odd-number-of-ones? test-odd-number-of-ones?)
;;; #t
;;; > (odd-number-of-ones? "0")
;;; "stuck"
;;; > (odd-number-of-ones? "1")
;;; "yes"
;;; > (odd-number-of-ones? "001000")
;;; "yes"
;;; > (odd-number-of-ones? "00111000")
;;; "yes"
;;; > (odd-number-of-ones? "0010101000")
;;; "yes"
;;; > (odd-number-of-ones? "00101010001")
;;; "stuck"
;;; > (odd-number-of-ones? "xy")
;;; "error"
;;; > 

;;;;;;;;;;

(define state
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (if (char=? (string-ref s i) #\0)
            (state s (1+ i) max b)
            (if (char=? (string-ref s i) #\1)
                (state s (1+ i) max (not b))
                "error")))))

(define other-odd-number-of-ones?
  (lambda (s)
    (state s 0 (string-length s) #f)))

;;; > (other-odd-number-of-ones? "")
;;; "stuck"
;;; > (other-odd-number-of-ones? "0")
;;; "stuck"
;;; > (other-odd-number-of-ones? "1")
;;; "yes"
;;; > (other-odd-number-of-ones? "001000")
;;; "yes"
;;; > (other-odd-number-of-ones? "00111000")
;;; "yes"
;;; > (other-odd-number-of-ones? "0010101000")
;;; "yes"
;;; > (other-odd-number-of-ones? "00101010001")
;;; "stuck"
;;; > (other-odd-number-of-ones? "xy")
;;; "error"
;;; > 

;;;;;;;;;;

(define number-of-leaves
  (lambda (v)
    (if (pair? v)
        (+ (number-of-leaves (car v))
           (number-of-leaves (cdr v)))
        1)))

;;; > (number-of-leaves 42)
;;; 1
;;; > (number-of-leaves number-of-leaves)
;;; 1
;;; > (number-of-leaves (cons 42 43))
;;; 2
;;; > (number-of-leaves (cons 42 number-of-leaves))
;;; 2
;;; > (number-of-leaves (cons (cons 1 2) (cons 3 4)))
;;; 4
;;; > (number-of-leaves (cons 0 (cons (cons 1 2) (cons 3 4))))
;;; 5
;;; > (number-of-leaves (cons (cons 0 (cons (cons 1 2) (cons 3 4))) (cons 5 6)))
;;; 7
;;; > 

(define test-number-of-leaves
  (lambda (candidate)
    (and (equal? (candidate 42)
                 1)
         (equal? (candidate candidate)
                 1)
         (equal? (candidate (cons 42 43))
                 2)
         (equal? (candidate (cons 42 number-of-leaves))
                 2)
         (equal? (candidate (cons (cons 1 2) (cons 3 4)))
                 4)
         (equal? (candidate (cons 0 (cons (cons 1 2) (cons 3 4))))
                 5)
         (equal? (candidate (cons (cons 0 (cons (cons 1 2) (cons 3 4))) (cons 5 6)))
                 7)
           ;;; add more tests here
         )))

;;; > (test-number-of-leaves number-of-leaves)
;;; #t
;;; > 

;;;;;;;;;;

(define test-number-of-pairs
  (lambda (candidate)
    (and (equal? (candidate (cons (cons 10 20) 30))
                 2)
         (equal? (candidate 10)
                 0)
         (equal? (candidate candidate)
                 0)
           ;;; add more tests here
         )))

(define number-of-pairs
  (lambda (v)
    "error"))

;;;;;;;;;;

(define test-equal?
  (lambda (candidate)
    (and
;;; numbers:
     (equal? (candidate 2
                        2)
             #t)
     (equal? (candidate 2
                        3)
             #f)
     (equal? (candidate 2
                        #t)
             #f)
;;; Booleans:
     (equal? (candidate #t
                        #t)
             #t)
     (equal? (candidate #f
                        #f)
             #t)
     (equal? (candidate #t
                        #f)
             #f)
     (equal? (candidate #t
                        33)
             #f)
;;; characters:
     (equal? (candidate #\c
                        #\c)
             #t)
     (equal? (candidate #\c
                        #\d)
             #f)
     (equal? (candidate #\c
                        33)
             #f)
;;; strings
     (equal? (candidate "hello"
                        "hello")
             #t)
     (equal? (candidate "hello"
                        "hola")
             #f)
     (equal? (candidate "hello"
                        33)
             #f)
     (equal? (candidate "42"
                        42)
             #f)
;;; procedures
     (equal? (candidate (lambda (x) x)
                        (lambda (y) y))
             #f)
;;; pairs
     (equal? (candidate (cons 1 2)
                        (cons 1 2))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 4)))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 4 3)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 2 1) (cons 3 4)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 #f)))
             #f)
;;; add more tests here
     )))

;;; > (test-equal? equal?)
;;; #t
;;; > 

(define equal?_revisited
  (lambda (v1 v2)
    (if (number? v1)
        (if (number? v2)
            (= v1 v2)
            #f)
        (if (boolean? v1)
            (if (boolean? v2)
                (boolean=? v1 v2)
                #f)
            (if (char? v1)
                (if (char? v2)
                    (char=? v1 v2)
                    #f)
                (if (string? v1)
                    (if (string? v2)
                        (string=? v1 v2)
                        #f)
                    (if (pair? v1)
                        (if (pair? v2)
                            (if (equal?_revisited (car v1) (car v2))
                                (equal?_revisited (cdr v1) (cdr v2))
                                #f)
                            #f)
                        #f)))))))

;;; > (test-equal? equal?_revisited)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-2.scm

"week-2.scm"
