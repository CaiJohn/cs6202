;;; week-8-apply.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-8-apply.html

;;;;;;;;;;

;;; Exercise 1
;;; ----------

(define foo
  (lambda (p xs)
    (errorf 'foo
            "not implemented yet")))

;;;;;;;;;;

;;; Exercise 2
;;; ----------

(define mystery-1
  (lambda (v)
    (apply (lambda (x . xs)
             x)
           v)))

(define mystery-2
  (lambda (v)
    (apply (lambda (x . xs)
             xs)
           v)))

;;;;;;;;;;

(define test-last-actual
  (lambda (candidate)
    (and (equal? (candidate 1) 1)
         (equal? (candidate 1 2) 2)
         (equal? (candidate 1 2 3) 3)
         ;;;
         )))

(define last-actual_v1
  (lambda actuals
    (if (null? actuals)
        (errorf 'last-actual_v1 "not enough actual parameters")
        (letrec ([visit (lambda (a as)
                          (if (null? as)
                              a
                              (visit (car as) (cdr as))))])
          (visit (car actuals) (cdr actuals))))))

(define last-actual_v2
  (lambda actuals
    (if (null? actuals)
        (errorf 'last-actual_v2 "not enough actual parameters")
        (list-ref actuals (1- (length actuals))))))

(define last-actual_v3
  (lambda actuals
    (if (null? actuals)
        (errorf 'last-actual_v3 "not enough actual parameters")
        (list-ref actuals (length (cdr actuals))))))

(define last-actual_v4
  (lambda actuals
    (cond
      [(null? actuals)
       (errorf 'last-actual_v3 "not enough actual parameters")]
      [(null? (cdr actuals))
       (car actuals)]
      [else
       (apply last-actual_v4 (cdr actuals))])))

(define last-actual_v5
  (lambda (actual . actuals)
    (if (null? actuals)
        actual
        (apply last-actual_v5 actuals))))

(define last-actual_v6
  (lambda (actual . actuals)
    (letrec ([visit (lambda (a as)
                      (if (null? as)
                          a
                          (visit (car as) (cdr as))))])
      (visit actual actuals))))

(define test-all-the-versions-of-last-actual
  (lambda ()
    (list (list 'last-actual_v1 ': (test-last-actual last-actual_v1))
          (list 'last-actual_v2 ': (test-last-actual last-actual_v2))
          (list 'last-actual_v3 ': (test-last-actual last-actual_v3))
          (list 'last-actual_v4 ': (test-last-actual last-actual_v4))
          (list 'last-actual_v5 ': (test-last-actual last-actual_v5))
          (list 'last-actual_v6 ': (test-last-actual last-actual_v6)))))

;;;;;;;;;;

(define and-all
  (lambda bs_init
    (letrec ([visit (lambda (bs)
                      (or (null? bs)
                          (and (car bs)
                               (visit (cdr bs)))))])
      (visit bs_init))))

(define try-candidate-variadically
  (lambda (name candidate expected-output . input)
    (or (equal? (apply candidate input)
                expected-output)
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define test-try-candidate-variadically
  (lambda (candidate)
    (and-all (candidate '+ + 3 1 2)
             (candidate '+ + 6 1 2 3)
             (candidate '* * 1)
             (candidate '* * 2 2)
             (candidate '* * 6 2 3)
             (candidate '* * 6 1 2 3)
             (candidate '* * 120 1 2 3 4 5)
               ;;;
             )))

;;; > (test-try-candidate-variadically try-candidate-variadically)
;;; #t
;;; > 

;;;;;;;;;;

(define negative-test-try-candidate-variadically
  (lambda (candidate)
    (and-all (candidate '+ + 0 1 2)
             (candidate '+ + 0 1 2 3)
             (candidate '* * 0)
             (candidate '* * 0 2)
             (candidate '* * 0 2 3)
             (candidate '* * 0 1 2 3)
             (candidate '* * 0 1 2 3 4 5)
               ;;;
             )))

;;; > (negative-test-try-candidate-variadically try-candidate-variadically)
;;; *: error for (1 2 3 4 5)
;;; *: error for (1 2 3)
;;; *: error for (2 3)
;;; *: error for (2)
;;; *: error for ()
;;; +: error for (1 2 3)
;;; +: error for (1 2)
;;; #f
;;; > 

;;;;;;;;;;

;;; end of week-8-apply.scm

"week-8-apply.scm"
