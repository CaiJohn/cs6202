;;; week-6-arithmetic-expressions-sample.scm
;;; was:
;;; sample-of-arithmetic-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; complete version of 17 Sep 2015

;;;;;;;;;;

(define source-ae0
  '42)

(define source-ae1
  '(+ 1 10))

(define source-ae2
  '(+ (+ 1 10)
      (+ 100 1000)))

(define source-ae3
  '(* (* (* 1 2)
         3)
      (* 4 5)))

(define source-ae4
 '(+ (+ 0 1) (+ 2 3)))

(define source-ae5
  '(+ (+ 0 1) (+ (* 2 3) (* 4 5))))

(define source-ae6
  '(+ (+ 0 1) (+ (+ (* (* 2 3) (* 4 5)) 6) (+ 7 8))))

(define source-ae7
  '(+ (+ 0 1) (+ (+ (* (* 2 (+ (+ 3 33) 333)) (* 4 5)) 6) (+ 7 8))))

(define source-ae8
  '(+ (* 1 10) (* 2 20)))

(define source-ae9
  '(+ (* 1 10) (* 2 20)))

(define source-ae10
  '(* (* (* 1 2)
         0)
      (* 4 5)))

(define source-ae11
  '(* (* (* 1 1)
         1)
      (* 1 1)))

(define source-ae12
  '(+ (* 0 1) (+ (+ (* (* 2 (+ (+ 3 33) (+ 0 333))) (* 4 5)) 6) (+ 7 8))))

(define source-ae13
  '(+ (* 1 0) (+ (+ (* (* 2 (+ (+ 3 33) (+ 333 0))) (* 4 5)) 6) (+ 7 8))))

(define source-ae14
  '(* (+ 2 20)
      (+ 200 2000)))

(define source-ae15
  '(* (+ 1 10)
      (+ 100 1000)))

;;;;;;;;;;

(define sample-of-arithmetic-expressions
  (list source-ae0
        source-ae1
        source-ae2
        source-ae3
        source-ae4
        source-ae5
        source-ae6
        source-ae7
        source-ae8
        source-ae9
        source-ae10
        source-ae11
        source-ae12
        source-ae13
        source-ae14
        source-ae15))

;;;;;;;;;;

;;; end of week-6-arithmetic-expressions-sample.scm

"week-6-arithmetic-expressions-sample.scm"
