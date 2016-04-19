;;; week-9-lazy-programming.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 Oct 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-9-lazy-programming.html

;;;;;;;;;;

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

(define test-candidate
  (lambda (name candidate expected-output . input)
    (or (equal? (apply candidate input)
                expected-output)
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

;;;;;;;;;;

(define conjunction
  (lambda (b1 b2)
    (if b1
        (if b2
            #t
            #f)
        #f)))

;;;;;;;;;;

(define force!
  (lambda (thunk)
    (thunk)))

(define delayed-conjunction
  (lambda (thunk_1 thunk_2)
    (if (force! thunk_1)
        (if (force! thunk_2)
            #t
            #f)
        #f)))

;;;;;;;;;;

(define stream-of-natural-numbers
  (letrec ([produce (lambda (current-natural-number)
                      (cons current-natural-number
                            (lambda ()
                              (produce (1+ current-natural-number)))))])
    (produce 0)))

(define test-stream-ref
  (lambda (candidate)
    (and-all (test-candidate 'stream-ref
                             candidate
                             0
                             stream-of-natural-numbers
                             0)
             (test-candidate 'stream-ref
                             candidate
                             10
                             stream-of-natural-numbers
                             10)
             (let ([i (random 100)])
               (test-candidate 'stream-ref
                               candidate
                               i
                               stream-of-natural-numbers
                               i))
             ;;; add more tests here
             )))

(define stream-ref
  (lambda (s_init n_init)
    (letrec ([visit (lambda (s n)
                      (if (zero? n)
                          (car s)
                          (visit (force! (cdr s))
                                 (1- n))))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (visit s_init n_init)
          (errorf 'stream-ref
                  "not a non-negative integer: ~s"
                  n_init)))))

;;; > (test-stream-ref stream-ref)
;;; #t
;;; > 

(unless (test-stream-ref stream-ref)
  (printf "(test-stream-ref stream-ref) failed~n"))

(define test-stream-head
  (lambda (candidate)
    (and-all (test-candidate 'stream-head
                             candidate
                             (iota 0)
                             stream-of-natural-numbers
                             0)
             (test-candidate 'stream-head
                             candidate
                             (iota 10)
                             stream-of-natural-numbers
                             10)
             (let ([i (random 100)])
               (test-candidate 'stream-head
                               candidate
                               (iota i)
                               stream-of-natural-numbers
                               i))
             ;;; add more tests here
             )))

(define stream-head
  (lambda (s_init n_init)
    (letrec ([visit (lambda (s n)
                      (if (zero? n)
                          '()
                          (cons (car s)
                                (visit (force! (cdr s))
                                       (1- n)))))])
      (if (and (integer? n_init)
               (not (negative? n_init)))
          (visit s_init n_init)
          (errorf 'stream-head
                  "not a non-negative integer: ~s"
                  n_init)))))

;;; > (test-stream-head stream-head)
;;; #t
;;; > 

(unless (test-stream-head stream-head)
  (printf "(test-stream-head stream-head) failed~n"))

(define make-stream
  (lambda (seed next)
    (letrec ([produce (lambda (current)
                        (cons current
                              (lambda ()
                                (produce (next current)))))])
      (produce seed))))

(define make-traced-stream
  (lambda (seed next)
    (letrec ([produce (trace-lambda produce (current)
                        (cons current
                              (lambda ()
                                (produce (next current)))))])
      (produce seed))))

;;;;;;;;;;

;;; Exercise 1

(define test-twice-the-stream
  (lambda (candidate)
    (and (equal? (stream-head (candidate stream-of-natural-numbers)
                              10)
                 '(0 2 4 6 8 10 12 14 16 18))
         (equal? (stream-head (candidate stream-of-even-natural-numbers)
                              10)
                 '(0 4 8 12 16 20 24 28 32 36))
         (equal? (stream-head (candidate stream-of-odd-natural-numbers)
                              10)
                 '(2 6 10 14 18 22 26 30 34 38))
         ;;; etc.
         )))

;;;;;;;;;;

;;; Exercise 3

(define test-dilute-stream
  (lambda (candidate)
    (and (equal? (stream-head (candidate stream-of-natural-numbers)
                              10)
                 '(0 2 4 6 8 10 12 14 16 18))
         (equal? (stream-head (candidate stream-of-even-natural-numbers)
                              10)
                 '(0 4 8 12 16 20 24 28 32 36))
         (equal? (stream-head (candidate stream-of-odd-natural-numbers)
                              10)
                 '(1 5 9 13 17 21 25 29 33 37))
         ;;; etc.
         )))

;;;;;;;;;;

;;; Exercise 4

(define test-merge-streams
  (lambda (candidate)
    (and (equal? (stream-head (candidate stream-of-even-natural-numbers
                                         stream-of-odd-natural-numbers)
                              10)
                 '(0 1 2 3 4 5 6 7 8 9))
         (equal? (stream-head (candidate stream-of-odd-natural-numbers
                                         stream-of-even-natural-numbers)
                              10)
                 '(1 0 3 2 5 4 7 6 9 8))
         ;;; etc.
         )))

;;;;;;;;;;

(define make-traced-stream
  (lambda (seed next)
    (letrec ([produce (trace-lambda produce (current)
                        (cons current
                              (lambda ()
                                (produce (next current)))))])
      (produce seed))))

;;; > (define traced-stream-of-natural-numbers
;;;     (make-traced-stream 0 1+))
;;; |(produce 0)
;;; |(0 . #<procedure>)
;;; > (stream-head traced-stream-of-natural-numbers 5)
;;; |(produce 1)
;;; |(1 . #<procedure>)
;;; |(produce 2)
;;; |(2 . #<procedure>)
;;; |(produce 3)
;;; |(3 . #<procedure>)
;;; |(produce 4)
;;; |(4 . #<procedure>)
;;; |(produce 5)
;;; |(5 . #<procedure>)
;;; (0 1 2 3 4)
;;; > (stream-head traced-stream-of-natural-numbers 3)
;;; |(produce 1)
;;; |(1 . #<procedure>)
;;; |(produce 2)
;;; |(2 . #<procedure>)
;;; |(produce 3)
;;; |(3 . #<procedure>)
;;; (0 1 2)
;;; > 

;;;;;;;;;;

(define test-samefringe
  (lambda (candidate)
    (and-all ;;; positive:
             (test-candidate 'samefringe
                             candidate
                             #t
                             1
                             1)
             (test-candidate 'samefringe
                             candidate
                             #t
                             1
                             '(((1))))
             (test-candidate 'samefringe
                             candidate
                             #t
                             '(((1)) . 2)
                             '(1 ((2))))
             (test-candidate 'samefringe
                             candidate
                             #t
                             '(((1) 2) 3)
                             '(1 2 . 3))
             ;;; negative:
             (test-candidate 'samefringe
                             candidate
                             #f
                             1
                             2)
             (test-candidate 'samefringe
                             candidate
                             #f
                             '(((1) 2) 3)
                             '(1 0 . 3))
             (test-candidate 'samefringe
                             candidate
                             #f
                             '(((1) 2) (3 . 4))
                             '(1 2 3))
             )))

(define flatten-binary-tree
  (lambda (binary-tree)
    (letrec ([visit (lambda (bt a)
                      (cond
                        [(number? bt)
                         (cons bt a)]
                        [(pair? bt)
                         (visit (car bt)
                                (if (null? (cdr bt))
                                    a
                                    (visit (cdr bt) a)))]
                        [else
                         (errorf 'flatten-binary-tree
                                 "not a binary tree of integers: ~s"
                                 bt)]))])
      (visit binary-tree '()))))

(define samefringe-naive
  (lambda (binary-tree-1 binary-tree-2)
    (equal? (flatten-binary-tree binary-tree-1)
            (flatten-binary-tree binary-tree-2))))

;;; > (test-samefringe samefringe-naive)
;;; #t
;;; > 

(unless (test-samefringe samefringe-naive)
  (printf "(test-samefringe samefringe-naive) failed~n"))

;;;;;;;;;;

(define flatten-binary-tree-lazily
  (lambda (binary-tree)
    (letrec ([visit (lambda (bt thunk)
                      (cond
                        [(number? bt)
                         (cons bt thunk)]
                        [(pair? bt)
                         (visit (car bt)
                                (if (null? (cdr bt))
                                    thunk
                                    (lambda ()
                                      (visit (cdr bt) thunk))))]
                        [else
                         (errorf 'flatten-binary-tree-lazily
                                 "not a binary tree of integers: ~s"
                                 bt)]))])
      (visit binary-tree (lambda () '())))))

(define samefringe
  (lambda (binary-tree-1 binary-tree-2)
    (letrec ([compare (lambda (p1 p2)
                        (and (= (car p1) (car p2))
                             (let ([r1 (force! (cdr p1))]
                                   [r2 (force! (cdr p2))])
                               (if (null? r1)
                                   (null? r2)
                                   (and (pair? r2)
                                        (compare r1 r2))))))])
      (compare (flatten-binary-tree-lazily binary-tree-1)
               (flatten-binary-tree-lazily binary-tree-2)))))

(unless (test-samefringe samefringe)
  (printf "(test-samefringe samefringe) failed~n"))

;;; A solution for Exercise 9:

(define samefringe-traced
  (lambda (binary-tree-1 binary-tree-2)
    (letrec ([compare (trace-lambda compare (p1 p2)
                        (and (= (car p1) (car p2))
                             (let ([r1 (force! (cdr p1))]
                                   [r2 (force! (cdr p2))])
                               (if (null? r1)
                                   (null? r2)
                                   (and (pair? r2)
                                        (compare r1 r2))))))])
      (compare (flatten-binary-tree-lazily binary-tree-1)
               (flatten-binary-tree-lazily binary-tree-2)))))

;;; > (samefringe-traced '((((1 (10 2) 100)))) '(((((1 11))) 2) . 100))
;;; |(compare (1 . #<procedure>) (1 . #<procedure>))
;;; |(compare (10 . #<procedure>) (11 . #<procedure>))
;;; |#f
;;; #f
;;; > (samefringe-traced '(1 2) '(1 2 3))
;;; |(compare (1 . #<procedure>) (1 . #<procedure>))
;;; |(compare (2 . #<procedure>) (2 . #<procedure>))
;;; |#f
;;; #f
;;; > (samefringe-traced '(1 2 3) '(1 2))
;;; |(compare (1 . #<procedure>) (1 . #<procedure>))
;;; |(compare (2 . #<procedure>) (2 . #<procedure>))
;;; |#f
;;; #f
;;; > (samefringe-traced '(1 (10 (((((2))))) 100)) '((1 10) 2 . 100))
;;; |(compare (1 . #<procedure>) (1 . #<procedure>))
;;; |(compare (10 . #<procedure>) (10 . #<procedure>))
;;; |(compare (2 . #<procedure>) (2 . #<procedure>))
;;; |(compare (100 . #<procedure>) (100 . #<procedure>))
;;; |#t
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-9-lazy-programming.scm

"week-9-lazy-programming.scm"
