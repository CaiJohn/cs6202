;;; week-5-random-programming.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 07 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-5-random-programming.html

;;;;;;;;;;

(define random-test-fac
  (lambda (fac1 fac2 max)
    (let ([n (random max)])
      (or (= (fac1 n)
             (fac2 n))
          n))))

(define random-test-fac-repeatedly
  (lambda (fac1 fac2 max how-many-times)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          #t
                          (let ([result (random-test-fac fac1 fac2 max)])
                            (if (number? result)
                                result
                                (visit (1- i))))))])
      (visit how-many-times))))

(define fac
  (lambda (n)
    (letrec ([visit (lambda (n)
                      (if (zero? n)
                          1
                          (* n (visit (1- n)))))])
      (visit n))))

(define fac_alt
  (lambda (n)
    (letrec ([visit (lambda (n a)
                      (if (zero? n)
                          a
                          (visit (1- n) (* n a))))])
      (visit n 1))))

;;; > (random-test-fac-repeatedly fac fac_alt 20 100)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-random-2
  (lambda (size-of-sample)
    (letrec ([loop (lambda (i zeroes ones)
                     (if (= i 0)
                         (list (* (/ zeroes size-of-sample) 100.0)
                               (* (/ ones size-of-sample) 100.0))
                         (let ([random-number (random 2)])
                           (case random-number
                             [(0)
                              (loop (- i 1) (+ zeroes 1) ones)]
                             [(1)
                              (loop (- i 1) zeroes (+ ones 1))]
                             [else
                              (errorf 'sample-random-2
                                      "~s is not an appropriate random number"
                                      random-number)]))))])
      (loop size-of-sample 0 0))))

;;; > (sample-random-2 10)
;;; (50.0 50.0)
;;; > (sample-random-2 100)
;;; (53.0 47.0)
;;; > (sample-random-2 1000)
;;; (50.3 49.7)
;;; > (sample-random-2 10000)
;;; (49.38 50.62)
;;; > (sample-random-2 100000)
;;; (50.027 49.973)
;;; > (sample-random-2 1000000)
;;; (49.9981 50.0019)
;;; > 

(define defensive-sample-random-2
  (lambda (size-of-sample)
    (cond
      [(not (and (integer? size-of-sample)
                 (positive? size-of-sample)))
       (errorf 'sample-random-2
               "not a positive integer: ~s"
               size-of-sample)]
      [else
       (sample-random-2 size-of-sample)])))

;;;;;;;;;;

(define positive-test-of-equality-within-a-percent-of-the-mean
  (lambda (candidate)
    (and (candidate 100 100 0)  ;;; 100 is equal to 100 within 0% of their mean

         (candidate 100 101 1)  ;;; 100 is equal to 101 within 1% of their mean
         (candidate 101 100 1)  ;;; and vice-versa

         (candidate 100 102 2)  ;;; 100 is equal to 102 within 2% of their mean
         (candidate 102 100 2)  ;;; and vice-versa

         (candidate 99 101 2)   ;;; 99 is equal to 101 within 2% of their mean
         (candidate 101 99 2)   ;;; and vice-versa

         (candidate 99 101 3)   ;;; 99 is also equal to 101 within 3% of their mean
         (candidate 101 99 3)   ;;; and vice-versa

         (candidate 49 51 4)    ;;; 49 is equal to 51 within 4% of their mean
         (candidate 51 49 4)    ;;; and vice-versa

         (candidate 12345 12354 5)
         ;;;
         )))

(define negative-test-of-equality-within-a-percent-of-the-mean
  (lambda (candidate)
    (not (or (candidate 99 101 1)   ;;; 99 is not equal to 101 within 1% of their mean
             (candidate 101 99 1)   ;;; and vice-versa

             (candidate 100 102 1)  ;;; 100 is not equal to 102 within 1% of their mean
             (candidate 102 100 1)  ;;; and vice-versa

             (candidate 100 103 2)  ;;; 100 is not equal to 103 within 2% of their mean
             (candidate 103 100 2)  ;;; and vice-versa

             ;;; 49 is not equal to 51 within less than 2%:
             (candidate 49 51 1)
             (candidate 49 51 1.5)
             (candidate 49 51 1.9)
             (candidate 49 51 1.9999)

             ;;; and vice versa:
             (candidate 51 49 1)
             (candidate 51 49 1.5)
             (candidate 51 49 1.9)
             (candidate 51 49 1.9999)

             ;;;
             ))))

;;;;;;;;;;

(define test-arithmetic-distance
  (lambda (candidate)
    (and (= (candidate 3 5) 2)   ;;; the distance between 3 and 5 is 2
         (= (candidate 5 3) 2)   ;;; and vice versa
         
         (= (candidate 1 7) 6)   ;;; the distance between 1 and 7 is 6
         (= (candidate 7 1) 6)   ;;; and vice versa
         
         (= (candidate 2 2) 0)   ;;; the distance between 2 and 2 is 0
         ;;;
         )))

(define arithmetic-distance
  (lambda (n1 n2)
    (abs (- n1 n2))))

;;; > (test-arithmetic-distance arithmetic-distance)
;;; #t
;;; > 

;;;;;;;;;;

(define test-arithmetic-mean
  (lambda (candidate)
    (and ;;; 4 is equidistant from 3 and 5 and vice-versa
         (= (distance 3 4) (distance 4 5))
         (= (candidate 3 5) 4)
         (= (candidate 5 3) 4)
     
         ;;; 5 is equidistant from 2 and 8 and vice-versa
         (= (distance 2 5) (distance 5 8))
         (= (candidate 2 8) 5)
         (= (candidate 8 2) 5)
     
         ;;; 2 is equidistant from itself
         (= (distance 2 2) (distance 2 2))
         (= (candidate 2 2) 2)

         ;;;
         )))

(define arithmetic-mean
  (lambda (n1 n2)
    (/ (+ n1 n2) 2)))

;;; > (test-arithmetic-mean arithmetic-mean)
;;; #t
;;; > 

;;;;;;;;;;

(define =_%
  (lambda (n1 n2 percent)
    (<= (distance n1 n2)
        (* (arithmetic-mean n1 n2) (/ percent 100)))))

;;; > (positive-test-of-equality-within-a-percent-of-the-mean =_%)
;;; #t
;;; > (negative-test-of-equality-within-a-percent-of-the-mean =_%)
;;; #t
;;; > 

;;; Exercise 2
;;; ----------

(define defensive-=_%-with-nested-if-expressions
  (lambda (n1 n2 percent)
    (if (real? n1)
        (if (real? n2)
            (if (and (real? percent)
                     (<= 0 percent)
                     (<= percent 100))
                (=_% n1 n2 percent)
                (errorf '=_%
                        "invalid percentage: ~s"
                        percent))
            (errorf '=_%
                    "invalid second number: ~s"
                    n2))
        (errorf '=_%
                "invalid first number: ~s"
                n1))))

;;; > (positive-test-of-equality-within-a-percent-of-the-mean defensive-=_%-with-nested-if-expressions)
;;; #t
;;; > (negative-test-of-equality-within-a-percent-of-the-mean defensive=_%-with-nested-if-expressions)
;;; #t
;;; > 

(define defensive-=_%-with-one-cond-expression
  (lambda (n1 n2 percent)
    (cond
      [(not (real? n1))
       (errorf '=_%
               "invalid first number: ~s"
               n1)]
      [(not (real? n2))
       (errorf '=_%
               "invalid second number: ~s"
               n2)]
      [(not (and (real? percent)
                 (<= 0 percent)
                 (<= percent 100)))
       (errorf '=_%
               "invalid percentage: ~s"
               percent)]
      [else
       (=_% n1 n2 percent)])))

;;; > (positive-test-of-equality-within-a-percent-of-the-mean defensive=_%-with-one-cond-expression)
;;; #t
;;; > (negative-test-of-equality-within-a-percent-of-the-mean defensive=_%-with-one-cond-expression)
;;; #t
;;; > 

;;;;;;;;;;

;;; Exercise 3
;;; ----------

(define ==_%
  (lambda (n1 n2 . optional)
    (cond
      [(null? optional)
       (=_% n1 n2 0)]
      [(null? (cdr optional))
       (=_% n1 n2 (car optional))]
      [else
       (errorf '==_%
               "too many arguments: ~s"
               optional)])))

;;; > (positive-test-of-equality-within-a-percent-of-the-mean ==_%)
;;; #t
;;; > (negative-test-of-equality-within-a-percent-of-the-mean ==_%)
;;; #t
;;; > 

(define extra-positive-test-of-equality-within-a-percent-of-the-mean
  (lambda (candidate)
    (and (candidate 1 1)
         (candidate 10 10)
         (candidate 100 100)
         (candidate 1000 1000)
         ;;;
         )))

;;; > (extra-positive-test-of-equality-within-a-percent-of-the-mean ==_%)
;;; #t
;;; > 

;;;;;;;;;;

(define positive-test-of-percented-equality-with-respect-to-the-mean
  (lambda (candidate)
    (and (candidate 100 100 100)   ;;; 100 is 100% equal to 100

         (candidate 100 101 99)    ;;; 100 is 99% equal to 101
         (candidate 101 100 99)    ;;; and vice-versa

         (candidate 100 102 98)    ;;; 100 is 98% equal to 102
         (candidate 102 100 98)    ;;; and vice-versa

         (candidate 99 101 98)     ;;; 99 is 98% equal to 101
         (candidate 101 99 98)     ;;; and vice-versa

         (candidate 99 101 97)     ;;; 99 is also 97% equal to 101
         (candidate 101 99 97)     ;;; and vice-versa

         (candidate 49 51 96)      ;;; 49 is 96% equal to 51
         (candidate 51 49 96)      ;;; and vice-versa

         ;;;
         )))

(define negative-test-of-percented-equality-with-respect-to-the-mean
  (lambda (candidate)
    (not (or (candidate 100 102 100)   ;;; 100 is not 100% equal to 102
             (candidate 102 100 100)   ;;; and vice-versa

             (candidate 99 101 99)     ;;; 99 is not 99% equal to 101
             (candidate 101 99 99)     ;;; and vice-versa

             (candidate 100 102 99)    ;;; 100 is not 99% equal to 102
             (candidate 102 100 99)    ;;; and vice-versa

             (candidate 100 103 98)    ;;; 100 is not 98% equal to 105
             (candidate 103 100 98)    ;;; and vice-versa

             (candidate 100 101 99.6)  ;;; 100 is not 99.6% equal to 101
             (candidate 101 100 99.6)  ;;; and vice-versa

             ;;; 49 is not equal to 51 at less than 98%:
             (candidate 49 51 99)
             (candidate 49 51 98.5)
             (candidate 49 51 98.1)
             (candidate 49 51 98.00001)

             ;;; and vice versa:
             (candidate 51 49 99)
             (candidate 51 49 98.5)
             (candidate 51 49 98.1)
             (candidate 51 49 98.00001)

             ;;;
             ))))

;;;;;;;;;;

;;; Exercise 4
;;; ----------

(define =^%
  (lambda (n1 n2 percent)
    (errorf '=^%
            "not implemented yet")))

;;; > (positive-test-of-percented-equality-with-respect-to-the-mean =^%)
;;; ...
;;; > (negative-test-of-percented-equality-with-respect-to-the-mean =^%)
;;; ...
;;; > 

;;;;;;;;;;

(define sample-random-2-within-a-percent-and-a-size-of-a-sample
  (lambda (percent size-of-sample)
    (letrec ([loop (lambda (i zeroes ones)
                     (if (= i 0)
                         (list (* (/ zeroes size-of-sample) 100.0)
                               (* (/ ones size-of-sample) 100.0)
                               (=_% zeroes ones percent))   ;;; <---***---
                         (let ([random-number (random 2)])
                           (case random-number
                             [(0)
                              (loop (- i 1) (+ zeroes 1) ones)]
                             [(1)
                              (loop (- i 1) zeroes (+ ones 1))]
                             [else
                              (errorf 'sample-random-2-within-a-percent-and-a-size-of-a-sample
                                      "~s is not an appropriate random number"
                                      random-number)]))))])
      (loop size-of-sample 0 0))))

;;; > (sample-random-2-within-a-percent-and-a-size-of-a-sample 5 10000)
;;; (49.69 50.31 #t)
;;; > (sample-random-2-within-a-percent-and-a-size-of-a-sample 5 100000)
;;; (50.005 49.995 #t)
;;; > (sample-random-2-within-a-percent-and-a-size-of-a-sample 5 1000000)
;;; (50.0085 49.9915 #t)
;;; > 

(define defensive-sample-random-2-within-a-percent
  (lambda (percent size-of-sample)
    (cond
      [(not (and (real? percent)
                 (<= 0 percent)
                 (<= percent 100)))
       (errorf 'sample-random-2-within-a-percent
               "invalid percentage: ~s"
               percent)]
      [(not (and (integer? size-of-sample)
                 (positive? size-of-sample)))
       (errorf 'sample-random-2-within-a-percent
               "not a positive integer: ~s"
               size-of-sample)]
      [else
       (sample-random-2-within-a-percent percent size-of-sample)])))

;;;;;;;;;;

(define sample-flip-within-a-percent-given-a-size-of-sample
  (lambda (flip percent size-of-sample)
    (letrec ([loop (lambda (i zeroes ones)
                     (if (= i 0)
                         (list (* (/ zeroes size-of-sample) 100.0)
                               (* (/ ones size-of-sample) 100.0)
                               (=_% zeroes ones percent))
                         (let ([random-number (flip)])   ;;; <---***---
                           (case random-number
                             [(0)
                              (loop (- i 1) (+ zeroes 1) ones)]
                             [(1)
                              (loop (- i 1) zeroes (+ ones 1))]
                             [else
                              (errorf 'sample-flip-within-a-percent-given-a-size-of-sample
                                      "~s is not an appropriate random number"
                                      random-number)]))))])
      (loop size-of-sample 0 0))))

(define global-size-of-sample 10000)

(define sample-flip-within-a-percent
  (lambda (flip percent . optionals)
    (let ([size-of-sample (cond
                            [(null? optionals)
                             global-size-of-sample]
                            [(null? (cdr optionals))
                             (car optionals)]
                            [else
                             (errorf 'sample-flip-within-a-percent
                                     "too many arguments: ~s"
                                     optionals)])])
      (sample-flip-within-a-percent-given-a-size-of-sample flip percent size-of-sample))))

(define sample-random-2-within-a-percent-and-a-size-of-a-sample_alt
  (lambda (percent size-of-sample)
    (sample-flip-within-a-percent-given-a-size-of-sample
     (lambda ()
       (random 2))
     percent
     size-of-sample)))

;;;;;;;;;;

;;; result: 0 or 1, with fairness between 0% (exclusively) and 100% (inclusively)
(define flip-with-a-given-fairness-in-a-given-ball-park
  (lambda (fairness ball-park)
    ;;; fairness: between 0% (exclusively) and 100% (inclusively)
    ;;; ball-park: a positive integer
    (let* ([size-decrease (* ball-park (/ (- 100 fairness) 100))]
           [actual-ball-park (- ball-park size-decrease)])
      (if (< (random ball-park)
             (/ actual-ball-park 2))
          0
          1))))

(define make-flip
  (lambda (fairness)
    (lambda ()
      (defensive-flip-with-a-given-fairness-in-a-given-ball-park fairness global-ball-park))))

;;; > (sample-flip-within-a-percent (make-flip 100) 10)
;;; (50.42 49.58 #t)
;;; > (sample-flip-within-a-percent (make-flip 90) 10)
;;; (44.58 55.42 #f)
;;; > (sample-flip-within-a-percent (make-flip 80) 10)
;;; (40.0 60.0 #f)
;;; > (sample-flip-within-a-percent (make-flip 70) 10)
;;; (34.08 65.92 #f)
;;; > (sample-flip-within-a-percent (make-flip 66.6666666) 10)
;;; (33.36 66.64 #f)
;;; > (sample-flip-within-a-percent (make-flip 60) 10)
;;; (30.89 69.11 #f)
;;; > (sample-flip-within-a-percent (make-flip 50) 10)
;;; (24.5 75.5 #f)
;;; > (sample-flip-within-a-percent (make-flip 40) 10)
;;; (20.11 79.89 #f)
;;; > (sample-flip-within-a-percent (make-flip 30) 10)
;;; (15.07 84.93 #f)
;;; > (sample-flip-within-a-percent (make-flip 20) 10)
;;; (9.71 90.29 #f)
;;; > (sample-flip-within-a-percent (make-flip 10) 10)
;;; (4.9 95.1 #f)
;;; > (sample-flip-within-a-percent (make-flip 5) 10)
;;; (2.46 97.54 #f)
;;; > (sample-flip-within-a-percent (make-flip 1) 10)
;;; (0.52 99.48 #f)
;;; > 

(define defensive-flip-with-a-given-fairness-in-a-given-ball-park
  (lambda (fairness ball-park)
    (cond
      [(not (and (real? fairness)
                 (< 0 fairness)
                 (<= fairness 100)))
       (errorf 'flip-with-a-given-fairness-in-a-given-ball-park
               "illegal fairness: ~s"
               fairness)]
      [(not (and (integer? ball-park)
                 (positive? ball-park)))
       (errorf 'flip-with-a-given-fairness-in-a-given-ball-park
               "not a positive integer: ~s"
               ball-park)]
      [else
       (flip-with-a-given-fairness-in-a-given-ball-park fairness ball-park)])))

;;;;;;;;;;

;;; Exercise 7
;;;; ----------

(define flip-with-a-given-fairness-in-a-given-ball-park_alt
  (lambda (fairness ball-park)
      ;;; fairness: between 0% (exclusively) and 100% (inclusively)
      ;;; ball-park: a positive integer
    (errorf 'flip-with-a-given-fairness-in-a-given-ball-park_alt
            "not implemented yet")))

;;;;;;;;;;

(define fair-flip
  (make-flip 100))

;;; > (sample-flip-within-a-percent fair-flip 5)
;;; (50.93 49.07 #t)
;;; > (sample-flip-within-a-percent fair-flip 5)
;;; (49.58 50.42 #t)
;;; > 

;;; Because the house always wins:

(define slightly-biased-flip
  (make-flip 95))

;;; > (sample-flip-within-a-percent slightly-biased-flip 5)
;;; (47.67 52.33 #f)
;;; > (sample-flip-within-a-percent slightly-biased-flip 5)
;;; (47.36 52.64 #f)
;;; > (sample-flip-within-a-percent slightly-biased-flip 10)
;;; (47.85 52.15 #t)
;;; > (sample-flip-within-a-percent slightly-biased-flip 10)
;;; (47.75 52.25 #t)
;;; > 

(define quite-biased-flip
  (make-flip 90))

;;; > (sample-flip-within-a-percent quite-biased-flip 15)
;;; (44.25 55.75 #f)
;;; > (sample-flip-within-a-percent quite-biased-flip 15)
;;; (45.25 54.75 #f)
;;; > 

(define seriously-biased-flip
  (make-flip 80))

(define horribly-biased-flip
  (make-flip 66))

(define monstruously-biased-flip
  (make-flip 50))

;;; > (sample-flip-within-a-percent monstruously-biased-flip 5)
;;; (24.25 75.75 #f)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 5)
;;; (24.14 75.86 #f)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 95)
;;; (25.28 74.72 #f)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 95)
;;; (24.36 75.64 #f)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 100)
;;; (24.61 75.39 #f)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 101)
;;; (25.01 74.99 #t)
;;; > (sample-flip-within-a-percent monstruously-biased-flip 101)
;;; (26.12 73.88 #t)
;;; > 

(define hopelessly-biased-flip
  (make-flip 5))

;;; > (sample-flip-within-a-percent hopelessly-biased-flip 100)
;;; (2.42 97.58 #f)
;;; > (sample-flip-within-a-percent hopelessly-biased-flip 100)
;;; (2.52 97.48 #f)
;;; > (sample-flip-within-a-percent hopelessly-biased-flip 200)
;;; (2.28 97.72 #t)
;;; > (sample-flip-within-a-percent hopelessly-biased-flip 200)
;;; (2.48 97.52 #t)
;;; > 

;;;;;;;;;;

(define make-fair-flip-out-of-any-flip
  (lambda (flip)
    (lambda ()
      (letrec ([loop (lambda ()
                       (let* ([r1 (flip)]
                              [r2 (flip)])
                         (if (= r1 r2)
                             (loop)
                             r1)))])
        (loop)))))

;;;;;;;;;;

;;; Exercise 10
;;; -----------

(define make-fair-flip-out-of-any-flip_optimized
  (lambda (flip)
    (lambda ()
      (letrec ([loop (lambda (r1)
                       (let ([r2 (flip)])
                         (if (= r1 r2)
                             (loop r2)
                             r1)))])
        (loop (flip))))))

;;;;;;;;;;

;;; Exercise 11
;;; -----------

(define make-fair-flip-out-of-any-flip_v3
  (lambda (flip)
    (lambda ()
      (letrec ([loop (lambda ()
                       (let ([r (flip)])
                         (if (= r (flip))
                             (loop)
                             r)))])
        (loop)))))

;;;;;;;;;;

;;; Exercise 12
;;; -----------

(define make-fair-flip-out-of-any-flip_v4
  (lambda (flip)
    (letrec ([loop (lambda ()
                     (let ([r (flip)])
                       (if (= r (flip))
                           (loop)
                           r)))])
      loop)))

;;;;;;;;;;

(define number-of-iterations-of-make-fair-flip-out-of-any-flip
  (lambda (flip)
    (lambda () 
      (letrec ([loop (lambda (i)
                       (let* ([r1 (flip)]
                              [r2 (flip)])
                         (if (= r1 r2)
                             (loop (1+ i))
                             i)))])
        (loop 0)))))

(define average
  (lambda (thunk how-many-times)
    (letrec ([loop (lambda (n a)
                     (if (= n 0)
                         (* (/ a how-many-times) 1.0)
                         (loop (1- n) (+ (thunk) a))))])
      (loop how-many-times 0))))

;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 100)) 100000)
;;; 1.00199
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 90)) 100000)
;;; 1.02484
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 80)) 100000)
;;; 1.08288
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 70)) 100000)
;;; 1.18932
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 60)) 100000)
;;; 1.38346
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 50)) 100000)
;;; 1.67127
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 40)) 100000)
;;; 2.11774
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 30)) 100000)
;;; 2.92002
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 20)) 100000)
;;; 4.55994
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 10)) 100000)
;;; 9.50657
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 5)) 100000)
;;; 19.49105
;;; > (average (number-of-iterations-of-make-fair-flip-out-of-any-flip (make-flip 1)) 100000)
;;; 99.47116
;;; > 

;;;;;;;;;;

;;; end of week-5-random-programming.scm

"week-5-random-programming.scm"
