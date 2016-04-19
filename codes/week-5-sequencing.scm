;;; week-5-sequencing.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 07 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-5-sequencing.html

;;;;;;;;;;

(define chatty-factorial
  (lambda (n)
    (begin
      (printf "(chatty-factorial ~s)~n" n)
      (if (= n 0)
          1
          (* n (chatty-factorial (1- n)))))))

;;;;;;;;;;

(define trace1
  (lambda (tag p)
    (lambda (x)
      (begin
        (printf "(~s ~s) ->~n" tag x)
        (let ([result (p x)])
          (begin
            (printf "(~s ~s) <- ~s~n" tag x result)
            result))))))

(define apply-a-unary-procedure-to-1-2-3
  (lambda (p)
    (let* ([r1 (p 1)]
           [r2 (p 2)]
           [r3 (p 3)])
      (list r1 r2 r3))))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define self-compose
  (lambda (p n_init)
    (letrec ([visit (lambda (n)
                      (if (= n 0)
                          (lambda (x)
                            x)
                          (compose p (visit (1- n)))))])
      (if (and (integer? n_init)
               (>= n_init 0))
          (visit n_init)
          (errorf 'self-compose
                  "not a positive integer: ~s"
                  n_init)))))

;;; > ((self-compose cdr 0) '(0 1 2 3 4 5 6 7 8 9))
;;; (0 1 2 3 4 5 6 7 8 9)
;;; > ((self-compose cdr 1) '(0 1 2 3 4 5 6 7 8 9))
;;; (1 2 3 4 5 6 7 8 9)
;;; > ((self-compose cdr 2) '(0 1 2 3 4 5 6 7 8 9))
;;; (2 3 4 5 6 7 8 9)
;;; > ((self-compose cdr 6) '(0 1 2 3 4 5 6 7 8 9))
;;; (6 7 8 9)
;;; > ((self-compose 1+ 0) 10)
;;; 10
;;; > ((self-compose 1+ 1) 10)
;;; 11
;;; > ((self-compose 1+ 2) 10)
;;; 12
;;; > ((self-compose 1+ 5) 10)
;;; 15
;;; > 

;;;;;;;;;;

(define trace2
  (lambda (tag p)
    (lambda (x y)
      (begin
        (printf "(~s ~s ~s) ->~n" tag x y)
        (let ([result (p x y)])
          (begin
            (printf "(~s ~s ~s) <- ~s~n" tag x y result)
            result))))))

(define apply-a-binary-procedure-to-1-2-3
  (lambda (p)
    (let* ([r1 (p 1 1)]
           [r2 (p 2 2)]
           [r3 (p 3 3)])
      (list r1 r2 r3))))


;;; > (apply-a-binary-procedure-to-1-2-3 +)
;;; (2 4 6)
;;; > 

;;;      > (apply-a-binary-procedure-to-1-2-3 (trace2 'add +))
;;;      (add 1 1) ->
;;;      (add 1 1) <- 2
;;;      (add 2 2) ->
;;;      (add 2 2) <- 4
;;;      (add 3 3) ->
;;;      (add 3 3) <- 6
;;;      (2 4 6)
;;;      > 

;;;;;;;;;;

(define test-fac-2.0
    (lambda (candidate)
      (and (or (equal? (candidate 0)
                       1)
               (errorf 'test-fac
                       "error for 0"))
           (or (equal? (candidate 1)
                       1)
               (errorf 'test-fac
                       "error for 1"))
           (or (equal? (candidate 5)
                       120)
               (errorf 'test-fac
                       "error for 5"))
           (or (equal? (candidate 6)
                       (* 1 2 3 4 5 6))
               (errorf 'test-fac
                       "error for 6"))
           ;;; add more tests here
           )))

;;;   > (test-fac-2.0 (lambda (x) x))
;;;   
;;;   Exception in test-fac: error for 0
;;;   Type (debug) to enter the debugger.
;;;   > (test-fac-2.0 fac)
;;;   #t
;;;   > (test-fac-2.0 fac_alt-lifted)
;;;   #t
;;;   > (test-fac-2.0 fac_alt)
;;;   #t
;;;   > 

;;;;;;;;;;

(define try-candidate
  (lambda (candidate input expected-output name)
    (or (equal? (candidate input)
                expected-output)
        (errorf name "error for ~s" input))))

(define test-fac-3.0
  (lambda (candidate)
    (and (try-candidate candidate 0 1 'test-fac)
         (try-candidate candidate 1 1 'test-fac)
         (try-candidate candidate 5 120 'test-fac)
         (try-candidate candidate 6 (* 1 2 3 4 5 6) 'test-fac)
           ;;;
         )))

;;; > (test-fac-3.0 (lambda (x) x))
;;; 
;;; Exception in test-fac: error for 0
;;; Type (debug) to enter the debugger.
;;; > (test-fac-3.0 fac)
;;; #t
;;; > (test-fac-3.0 fac_alt-lifted)
;;; #t
;;; > (test-fac-3.0 fac_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define fac-Loki
  (lambda (n)
    (case n
      [(0)
       1]
      [(1)
       1]
      [(5)
       120]
      [(6)
       (* 1 2 3 4 5 6)]
      [else
       -1])))

;;;;;;;;;;

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

;;; > (and-all (time #f) (time #f))
;;; (time #f)
;;;     no collections
;;;     0 ms elapsed cpu time
;;;     0 ms elapsed real time
;;;     0 bytes allocated
;;; (time #f)
;;;     no collections
;;;     0 ms elapsed cpu time
;;;     0 ms elapsed real time
;;;     0 bytes allocated
;;; #f
;;; > 

(define try-candidate-transparently
  (lambda (candidate input expected-output name)
    (or (equal? (candidate input)
                expected-output)
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define test-fac-3.1
  (lambda (candidate)
    (and-all (try-candidate-transparently candidate 0 1 'test-fac-3.1)
             (try-candidate-transparently candidate 1 1 'test-fac-3.1)
             (try-candidate-transparently candidate 5 120 'test-fac-3.1)
             (try-candidate-transparently candidate 6 (* 1 2 3 4 5 6) 'test-fac-3.1)
             ;;;
             )))

;;; > (test-fac-3.1 (lambda (x) x))
;;; test-fac-3.1: error for 6
;;; test-fac-3.1: error for 5
;;; test-fac-3.1: error for 0
;;; #f
;;; > 

;;;;;;;;;;

(define fac
  (lambda (n)
    (if (zero? n)
        1
        (* n (fac (1- n))))))

(define fac_acc
  (lambda (n a)
    (if (zero? n)
        a
        (fac_acc (1- n) (* n a)))))

(define fac_alt-lifted
  (lambda (n)
    (fac_acc n 1)))

(define fac_alt
  (lambda (n)
    (letrec ([visit (lambda (n a)
                      (if (zero? n)
                          a
                          (visit (1- n) (* n a))))])
      (visit n 1))))

;;; > (test-fac-3.1 fac)
;;; #t
;;; > (test-fac-3.1 fac_alt-lifted)
;;; #t
;;; > (test-fac-3.1 fac_alt)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-5-sequencing.scm

"week-5-sequencing.scm"
