;;; fibonacci.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 Dec 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-9-parameter-passing-strategies.html

;;;;;;;;;;

;;; The traditional Fibonacci procedure:

(define fib
  (lambda (v)
    (letrec ([visit (lambda (n)
                      (case n
                       [(0 1)
                        n]
                       [else
                        (+ (visit (- n 1))
                           (visit (- n 2)))]))])
      (if (and (integer? v)
               (>= v 0))
          (visit v)
          (errorf 'fib "not a natural number: ~s" v)))))

;;; List of the Fibonacci numbers:

(define fibonacci-numbers
  (letrec ([make-fibs (lambda (n)
                        (cons (fib n)
                              (make-fibs (+ n 1))))])
    (make-fibs 0)))

;;;;;;;;;;

;;; end of fibonacci.scm
