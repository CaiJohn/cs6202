;;; week-3.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 25 Aug 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-3.html

;;;;;;;;;;

;;; from week-1.scm:

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

;;;;;;;;;;

(define nil (list))

(define null nil)

;;;;;;;;;;

(define plus_revisited
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1 n2)
                      (if (= n1 0)
                          n2
                          (1+ (visit (1- n1) n2))))])
      (visit n1 n2))))

;;; > (test-plus plus_revisited)
;;; #t
;;; > 

;;;;;;;;;;

(define plus_revisited_alt
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1)
                      (if (= n1 0)
                          n2
                          (1+ (visit (1- n1)))))])
      (visit n1))))

;;; > (test-plus plus_revisited_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define plus_revisited_visit
  (lambda (n1 n2)
    (if (= n1 0)
        n2
        (1+ (plus_revisited_visit (1- n1) n2)))))

(define plus_revisited_lifted
  (lambda (n1 n2)
    (plus_revisited_visit n1 n2)))

;;; > (test-plus plus_revisited_lifted)
;;; #t
;;; > 

;;;;;;;;;;

(define one_or_the_other
  (lambda (n)
    (letrec ([even? (lambda (n)
                      (if (= n 0)
                          #t
                          (odd? (1- n))))]
             [odd? (lambda (n)
                     (if (= n 0)
                         #f
                         (even? (1- n))))])
      (list (even? n) (odd? n)))))

(define test-one_or_the_other
  (lambda (candidate)
    (and (equal? (candidate 0)
                 '(#t #f))
         (equal? (candidate 1)
                 '(#f #t))
         (equal? (candidate 8)
                 '(#t #f))
         (equal? (candidate 9)
                 '(#f #t))
           ;;;
         )))

;;; > (test-one_or_the_other one_or_the_other)
;;; #t
;;; > 

;;;;;;;;;;

(define one_or_the_other_revisited
  (lambda (n)
    (letrec ([even? (lambda (n)
                      (or (= n 0)
                          (odd? (1- n))))]
             [odd? (lambda (n)
                     (and (not (= n 0))
                          (even? (1- n))))])
      (list (even? n) (odd? n)))))

;;; > (test-one_or_the_other one_or_the_other_revisited)
;;; #t
;;; > 

;;;;;;;;;;

(define 246?
  (lambda (x)
    (if (member x '(2 4 6))
        #t
        #f)))

(define 246?_alt
  (lambda (x)
    (case x
      [(2 4 6)
       #t]
      [else
       #f])))

;;;;;;;;;;

;;; end of week-3.scm

"week-3.scm"
