;;; lists.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 Oct 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-9-parameter-passing-strategies.html

;;;;;;;;;;

(define list-a-ref
  (lambda (xs n)
    (if (= n 0)
        (car xs)
        (list-a-ref (cdr xs) (- n 1)))))

(define make-a-list
  (lambda (seed next)
    (letrec ([produce (lambda (current)
                        (begin
                          (display "producing ")
                          (display current)
                          (newline)
                          (cons current
                                (produce (next current)))))])
      (produce seed))))


(define make-a-list-of-nats
  (lambda (n)
    (make-a-list n
                 (lambda (n)
                   (+ n 1)))))

;;;;;;;;;;

;;; end of lists.scm

