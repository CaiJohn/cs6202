;;; week-3-lists-and-defensive-programming.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 25 Aug 2015

;;; Accompanying material for the lecture notes at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-3-lists-and-defensive-programming.html

;;;;;;;;;;

(define test-length_proper-list
  (lambda (candidate)
    (and (equal? (candidate (list))
                 0)
         (equal? (candidate (list 1 2 3))
                 3)
           ;;; add more tests here
         )))

;;; > (test-length_proper-list length)
;;; #t
;;; > 

(define length_proper-list
  (lambda (xs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                              ;;; base case:
                          0
                              ;;; induction case:
                          (1+ (visit (cdr ws)))))])
      (visit xs))))

;;; > (test-length_proper-list length_proper-list)
;;; #t
;;; > 

;;;;;;;;;;

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

;;; > (test-proper-list-append append)
;;; #t
;;; > 

(define append_proper-list
  (lambda (xs ys)
    (letrec ([visit (lambda (xs ys)
                      (if (null? xs)
                              ;;; base case:
                          ys
                              ;;; induction case:
                          (cons (car xs) (visit (cdr xs) ys))))])
      (visit xs ys))))

;;; > (test-append_proper-list append_proper-list)
;;; #t
;;; > 

;;;;;;;;;;

(define cautious-quotient
  (lambda (i j)
    (if (number? i)
        (if (number? j)
            (if (= j 0)
                (errorf 'cautious-quotient
                        "cannot divide anything by zero")
                (quotient i j))
            (errorf 'cautious-quotient
                    "~s is not a number"
                    j))
        (errorf 'cautious-quotient
                "~s is not a number"
                i))))

;;; > (cautious-quotient 10 4)
;;; 2
;;; > (cautious-quotient "10" 4)
;;; 
;;; Exception in cautious-quotient: "10" is not a number
;;; Type (debug) to enter the debugger.
;;; > (cautious-quotient 10 "4")
;;; 
;;; Exception in cautious-quotient: "4" is not a number
;;; Type (debug) to enter the debugger.
;;; > (cautious-quotient 10 0)
;;; 
;;; Exception in cautious-quotient: cannot divide anything by zero
;;; Type (debug) to enter the debugger.
;;; >

;;;;;;;;;;

(define safe-fac
  (lambda (n)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          1
                          (* i (visit (1- i)))))])
      (if (and (integer? n)
               (>= n 0))
          (visit n)
          (errorf 'safe-fac "not a positive integer: ~s" n)))))

;;; > (test-fac safe-fac)
;;; #t
;;; > 

;;;;;;;;;;

(define head-and-tail
  (lambda (vs i)
    (if (list? vs)
        (let ([n (length vs)])
          (if (and (integer? i)
                   (<= 0 i)
                   (<= i n))
              (equal? (append (proper-list-head vs i)
                              (proper-list-tail vs i))
                      vs)
              (errorf 'head-and-tail
                      "not an integer between 0 and ~s: ~s"
                      n
                      i)))
        (errorf 'head-and-tail
                "not a proper list: ~s"
                vs))))

;;; > (head-and-tail '(0 1 2 3 4 5 6 7 8) 0)
;;; #t
;;; > (head-and-tail '(0 1 2 3 4 5 6 7 8) 2)
;;; #t
;;; > (head-and-tail '(0 1 2 3 4 5 6 7 8) 5)
;;; #t
;;; > (head-and-tail '(0 1 2 3 4 5 6 7 8) 8)
;;; #t
;;; > (head-and-tail '(0 1 2 3 4 5 6 7 8) 9)
;;; #t
;;; >

(define test-proper-list-ref
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 0)
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 3)
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 5)
           ;;; 
         )))

;;; > (test-proper-list-ref list-ref)
;;; #t
;;; > 

(define test-proper-list-head
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 '())
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 '(0 1 2))
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 '(0 1 2 3 4))
         (equal? (candidate '(0 1 2 3 4 5) 6)
                 '(0 1 2 3 4 5))
           ;;; 
         )))

;;; > (test-proper-list-head list-head)
;;; #t
;;; > 

(define test-proper-list-tail
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 '(0 1 2 3 4 5))
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 '(3 4 5))
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 '(5))
         (equal? (candidate '(0 1 2 3 4 5) 6)
                 '())
           ;;; 
         )))

;;; > (test-proper-list-tail list-tail)
;;; #t
;;; > 

;;;;;;;;;;

(define set?
  (lambda (vs)
    (if (null? vs)
        #t
        (and (not (member (car vs) (cdr vs)))
             (set? (cdr vs))))))

(define s0_normalized '())
(define s0_a s0_normalized)

(define s1_normalized '(1))
(define s1_a s1_normalized)
(define s1_b '(1 1 1))

(define s2_normalized '(1 2 3 4 5))
(define s2_a s2_normalized)
(define s2_b '(1 1 1 2 1 2 1 3 4 5))
(define s2_c '(2 1 3 3 3 3 5 4))

;;; add more definitions here

(define test-set-normalize
  (lambda (candidate)
    (and (let ([result (candidate s0_a)])
           (and (set? result)
                (equal? s0_normalized
                        (sort < result))))

         (let ([result (candidate s1_a)])
           (and (set? result)
                (equal? s1_normalized
                        (sort < result))))
         (let ([result (candidate s1_b)])
           (and (set? result)
                (equal? s1_normalized
                        (sort < result))))

         (let ([result (candidate s2_a)])
           (and (set? result)
                (equal? s2_normalized
                        (sort < result))))
         (let ([result (candidate s2_b)])
           (and (set? result)
                (equal? s2_normalized
                        (sort < result))))
         (let ([result (candidate s2_c)])
           (and (set? result)
                (equal? s2_normalized
                        (sort < result))))

             ;;; add more tests here
         )))

(define set-normalize
  (lambda (v)
    (letrec ([visit (lambda (xs)
                      (cond
                        [(null? xs)
                         '()]
                        [(pair? xs)
                         (if (member (car xs) (cdr xs))
                             (visit (cdr xs))
                             (cons (car xs) (visit (cdr xs))))]
                        [else
                         (errorf 'set-normalize
                                 "not a proper list: ~s"
                                 v)]))])
      (visit v))))

;;; > (test-set-normalize set-normalize)
;;; #t
;;; > 

;;;;;;;;;;

(define s0 '())

(define s0_U_s0 s0)

(define s1 '(1 3 5 7))
(define s2 '(2 4 6 8))
(define s3 '(1 2 3 4))
(define s4 '(5 6 7 8))
;;; add more definitions here

(define s0_U_s1 s1)
(define s1_U_s0 s1)

(define s1_U_s1 s1)

(define s1_U_s2 '(1 2 3 4 5 6 7 8))
(define s1_U_s3 '(1 2 3 4 5 7))
;;; (define s1_U_s4 ...)

;;; (define s2_U_s3 ...)
;;; (define s2_U_s4 ...)

;;; (define s3_U_s4 ...)

(define s2_U_s1 '(1 2 3 4 5 6 7 8))
(define s3_U_s1 '(1 2 3 4 5 7))
;;; (define s4_U_s1 ...)

;;; (define s3_U_s2 ...)
;;; (define s4_U_s2 ...)

;;; (define s4_U_s3 ...)

;;; add more definitions here

(define test-set-union
  (lambda (candidate)
    (and (equal? s0_U_s0
                 (sort < (candidate s0 s0)))

         (equal? s0_U_s1
                 (sort < (candidate s0 s1)))
         (equal? s1_U_s0
                 (sort < (candidate s1 s0)))

         (equal? s1_U_s1
                 (sort < (candidate s1 s1)))

         (equal? s1_U_s2
                 (sort < (candidate s1 s2)))
         (equal? s1_U_s3
                 (sort < (candidate s1 s3)))
           ;;; add more tests here

         (equal? s2_U_s1
                 (sort < (candidate s2 s1)))
         (equal? s3_U_s1
                 (sort < (candidate s3 s1)))
           ;;; add more tests here

           ;;; keep adding
           ;;; be thorough,
           ;;; get a sense of all that you are testing
         )))

(define set-union
  (lambda (s1 s2)
    (errorf 'set-union "not implemented yet")))

;;;;;;;;;;

(define test-set-intersection
  (lambda (candidate)
    (errorf 'test-set-intersection
            "not implemented yet")))

(define set-intersection
  (lambda (s1 s2)
    (errorf 'set-intersection "not implemented yet")))

;;;;;;;;;;

(define test-set-difference
  (lambda (candidate)
    (errorf 'test-set-difference
            "not implemented yet")))

(define set-difference
  (lambda (s1 s2)
    (errorf 'set-difference "not implemented yet")))

;;;;;;;;;;

;;; end of week-3-lists-and-defensive-programming.scm

"week-3-lists-and-defensive-programming.scm"
