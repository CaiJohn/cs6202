;;; week-10-self-interpreter-continued-and-ended.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 30 Oct 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-10.html

;;;;;;;;;;

;;; Solution of Exercise 1:

(define make-tower
  (lambda (height)
    (letrec ([visit (lambda (n)
                      (if (= n 0)
                          '((lambda (x y z) (+ z z)) (time 1) (time 10) (time 100))
                          (list representation-of-interpret (list 'quote (visit (1- n))))))])
      (if (and (integer? height)
               (not (negative? height)))
          (visit height)
          (errorf 'make-tower
                  "not a non-negative integer: ~s"
                  height)))))

;;;;;;;;;;

;;; end of week-10-self-interpreter-continued-and-ended.scm

"week-10-self-interpreter-continued-and-ended.scm"
