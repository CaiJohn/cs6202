;;; week-11.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 27 Oct 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-11.html

;;;;;;;;;;;;;;;;;;;;

(define proper-list-of-fixed-length?
  (lambda (v n)
    (letrec ([visit (lambda (v n)
                      (if (= n 0)
                          (null? v)
                          (and (pair? v)
                               (visit (cdr v) (- n 1)))))])
      (if (and (integer? n)
               (>= n 0))
          (visit v n)
          (errorf 'proper-list-of-fixed-length?
                  "not a non-negative integer: ~s"
                  n)))))

;;;;;;;;;;;;;;;;;;;;

(define bar
  (lambda (f g)
    (list (f 1) (g 10 100))))

(define foo
  (lambda (x y z)
    (list (bar (lambda (a)
                 a)
               (lambda (b c)
                 (+ x y b c)))
          (bar (lambda (a)
                 (list a z))
               (lambda (b c)
                 (list b y))))))

(define test-foo
  (lambda (candidate)
    (and (equal? (candidate 0 0 0)
                 '((1 110) ((1 0) (10 0))))
         (equal? (candidate 1 1 1)
                 '((1 112) ((1 1) (10 1))))
         (equal? (candidate 1000 10000 100000)
                 '((1 11110) ((1 100000) (10 10000))))
         ;;;
         )))

(unless (test-foo foo)
  (printf "(test-foo foo) failed~n"))

;;;;;;;;;;

;;; constructors:

(define make-bar-1-1    ;;; (lambda (a) a)
  (lambda ()
    (list 'bar-1-1)))

(define make-bar-1-2    ;;; (lambda (a) (list a z))
  (lambda (z)
    (list 'bar-1-2 z)))

(define make-bar-2-1    ;;; (lambda (b c) (+ x y b c))
  (lambda (x y)
    (list 'bar-2-1 x y)))

(define make-bar-2-2    ;;; (lambda (b c) (list b y))
  (lambda (y)
    (list 'bar-2-2 y)))

;;; predicates:

(define is-bar-1-1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 1)
         (equal? (car v) 'bar-1-1))))

(define is-bar-1-2?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 2)
         (equal? (car v) 'bar-1-2))))

(define is-bar-2-1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (equal? (car v) 'bar-2-1))))

(define is-bar-2-2?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 2)
         (equal? (car v) 'bar-2-2))))

;;; accessors:

(define bar-1-2_1
  (lambda (v)
    (list-ref v 1)))

(define bar-2-1_1
  (lambda (v)
    (list-ref v 1)))

(define bar-2-1_2
  (lambda (v)
    (list-ref v 2)))

(define bar-2-2_1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; foo and bar, defunctionalized:

(define apply-bar-1
  (lambda (f a)
    (cond
      [(is-bar-1-1? f)    ;;; (lambda (a) a)
       a]
      [(is-bar-1-2? f)    ;;; (lambda (a) (list a z))
       (let ([z (bar-1-2_1 f)])
         (list a z))]
      [else
       (errorf 'apply-bar-1
               "not something defunctionalized: ~s"
               f)])))

(define apply-bar-2
  (lambda (g b c)
    (cond
      [(is-bar-2-1? g)    ;;; (lambda (b c) (+ x y b c))
       (let ([x (bar-2-1_1 g)]
             [y (bar-2-1_2 g)])
         (+ x y b c))]
      [(is-bar-2-2? g)    ;;; (lambda (b c) (list b y))
       (let ([y (bar-1-2_1 g)])
         (list b y))]
      [else
       (errorf 'apply-bar-2
               "not something defunctionalized: ~s"
               g)])))

(define bar_def
  (lambda (f g)
    (list (apply-bar-1 f 1) (apply-bar-2 g 10 100))))

(define foo_def
  (lambda (x y z)
    (list (bar_def (make-bar-1-1)       ;;; (lambda (a) a)
                   (make-bar-2-1 x y))  ;;; (lambda (a) (list a z))
          (bar_def (make-bar-1-2 z)     ;;; (lambda (b c) (+ x y b c))
                   (make-bar-2-2 y))))) ;;; (lambda (b c) (list b y))

(unless (test-foo foo_def)
  (printf "(test-foo foo_def) failed~n"))

;;;;;;;;;;;;;;;;;;;;

(define test-fac
  (lambda (candidate)
    (and (equal? (candidate 0)
                 1)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 5)
                 120)
         (equal? (candidate 7)
                 (* 1 2 3 4 5 6 7))
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; the factorial procedure in direct style:

(define fac_v0_aux
  (lambda (n)
    (if (zero? n)
        1
        (* n (fac_v0_aux (1- n))))))

(define fac_v0
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fac_v0_aux v)
        (errorf 'fac_v0
                "not a non-negative integer: ~s"
                v))))

(unless (test-fac fac_v0)
  (printf "(test-fac fac_v0) failed~n"))

;;;;;;;;;;

;;; the factorial procedure
;;; with all intermediate results named
;;; and their computation sequentialized:

(define fac_v1_aux
  (lambda (n)
    (if (zero? n)
        1
        (let ([x1 (fac_v1_aux (1- n))])
          (* n x1)))))

(define fac_v1
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fac_v1_aux v)
        (errorf 'fac_v1
                "not a non-negative integer: ~s"
                v))))

(unless (test-fac fac_v1)
  (printf "(test-fac fac_v1) failed~n"))

;;;;;;;;;;

;;; the factorial procedure
;;; in continuation-passing style:

(define fac_v2_aux
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (fac_v2_aux (1- n) (lambda (x1)
                             (k (* n x1)))))))

(define fac_v2
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fac_v2_aux v (lambda (a) a))
        (errorf 'fac_v2
                "not a non-negative integer: ~s"
                v))))

(unless (test-fac fac_v2)
  (printf "(test-fac fac_v2) failed~n"))

;;;;;;;;;;

;;; the factorial procedure
;;; in continuation-passing style,
;;; defunctionalized:

;;; constructors:

(define make-fac-k0     ;;; (lambda (a) a)
  (lambda ()
    (list 'fac-k0)))

(define make-fac-k1     ;;; (lambda (x1) (k (* n x1)))
  (lambda (n k)
    (list 'fac-k1 n k)))

;;; predicates:

(define is-fac-k0?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 1)
         (eqv? (car v) 'fac-k0))))

(define is-fac-k1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (eqv? (car v) 'fac-k1))))

;;; accessors:

(define fac-k1_1
  (lambda (v)
    (list-ref v 1)))

(define fac-k1_2
  (lambda (v)
    (list-ref v 2)))

;;; the apply procedure:

(define apply-fac-k
  (lambda (k v)
    (cond
      [(is-fac-k0? k)   ;;; (lambda (a) a)
       (let ([a v])
         a)]
      [(is-fac-k1? k)   ;;; (lambda (x1) (k (* n x1)))
       (let ([x1 v]
             [n (fac-k1_1 k)]
             [k (fac-k1_2 k)])
         (apply-fac-k k (* n x1)))]
      [else
       (errorf 'apply-fac-k
               "not a defunctionalized continuation: ~s"
               k)])))

(define fac_v3_aux
  (lambda (n k)
    (if (zero? n)
        (apply-fac-k k 1)
        (fac_v3_aux (1- n) (make-fac-k1 n k)))))

(define fac_v3
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fac_v3_aux v (make-fac-k0))
        (errorf 'fac_v3
                "not a non-negative integer: ~s"
                v))))

(unless (test-fac fac_v3)
  (printf "(test-fac fac_v3) failed~n"))

;;;;;;;;;;;;;;;;;;;;

(define test-fib
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 2)
                 1)
         (equal? (candidate 3)
                 2)
         (equal? (candidate 4)
                 3)
         (equal? (candidate 5)
                 5)
         (equal? (candidate 6)
                 8)
         (equal? (candidate 7)
                 13)
         (equal? (candidate 8)
                 21)
         (equal? (candidate 9)
                 34)
         ;;; add more tests, based on the Fibonacci sequence
         )))

;;;;;;;;;;

;;; the fibonacci procedure in direct style:

(define fib_v0_aux
  (lambda (n)
    (cond
      [(= n 0)
       0]
      [(= n 1)
       1]
      [else
       (+ (fib_v0_aux (- n 1))
          (fib_v0_aux (- n 2)))])))

(define fib_v0
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fib_v0_aux v)
        (errorf 'fib_v0
                "not a non-negative integer: ~s"
                v))))

(unless (test-fib fib_v0)
  (printf "(test-fib fib_v0) failed~n"))

;;;;;;;;;;

;;; the fibonacci procedure
;;; with all intermediate results named
;;; and their computation sequentialized:

(define fib_v1_aux
  (lambda (n)
    (cond
      [(= n 0)
       0]
      [(= n 1)
       1]
      [else
       (let* ([x1 (fib_v1_aux (- n 1))]
              [x2 (fib_v1_aux (- n 2))])
         (+ x1 x2))])))

(define fib_v1
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fib_v1_aux v)
        (errorf 'fib_v1
                "not a non-negative integer: ~s"
                v))))

(unless (test-fib fib_v1)
  (printf "(test-fib fib_v1) failed~n"))

;;;;;;;;;;

;;; the fibonacci procedure
;;; in continuation-passing style:

(define fib_v2_aux
  (lambda (n k)
    (cond
      [(= n 0)
       (k 0)]
      [(= n 1)
       (k 1)]
      [else
       (fib_v2_aux (- n 1)
                   (lambda (x1)
                     (fib_v2_aux (- n 2)
                                 (lambda (x2)
                                   (k (+ x1 x2))))))])))

(define fib_v2
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fib_v2_aux v (lambda (a) a))
        (errorf 'fib_v2
                "not a non-negative integer: ~s"
                v))))

(unless (test-fib fib_v2)
  (printf "(test-fib fib_v2) failed~n"))

;;;;;;;;;;

;;; the fibonacci procedure
;;; in continuation-passing style,
;;; defunctionalized:

;;; constructors:

(define make-fib-k0     ;;; (lambda (a) ...)
  (lambda ()
    (list 'fib-k0)))

(define make-fib-k1     ;;; (lambda (x1) ...)
  (lambda (n k)
    (list 'fib-k1 n k)))

(define make-fib-k2     ;;; (lambda (x2) ...)
  (lambda (k x1)
    (list 'fib-k2 k x1)))

;;; predicates:

(define is-fib-k0?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 1)
         (eqv? (car v) 'fib-k0))))

(define is-fib-k1?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (eqv? (car v) 'fib-k1))))

(define is-fib-k2?
  (lambda (v)
    (and (proper-list-of-fixed-length? v 3)
         (eqv? (car v) 'fib-k2))))

;;; accessors:

(define fib-k1_1
  (lambda (v)
    (list-ref v 1)))

(define fib-k1_2
  (lambda (v)
    (list-ref v 2)))

(define fib-k2_1
  (lambda (v)
    (list-ref v 1)))

(define fib-k2_2
  (lambda (v)
    (list-ref v 2)))

;;; the apply procedure:

(define apply-fib-k
  (lambda (k v)
    (cond
      [(is-fib-k0? k)   ;;; (lambda (a) ...)
       (let ([a v])
         a)]
      [(is-fib-k1? k)   ;;; (lambda (x1) ...)
       (let ([x1 v]
             [n (fib-k1_1 k)]
             [k (fib-k1_2 k)])
         (fib_v3_aux (- n 2) (make-fib-k2 k x1)))]
      [(is-fib-k2? k)   ;;; (lambda (x2) ...)
       (let ([x2 v]
             [k (fib-k2_1 k)]
             [x1 (fib-k2_2 k)])
         (apply-fib-k k (+ x1 x2)))]
      [else
       (errorf 'apply-fib-k
               "not a defunctionalized continuation: ~s"
               k)])))

(define fib_v3_aux
  (lambda (n k)
    (cond
      [(= n 0)
       (apply-fib-k k 0)]
      [(= n 1)
       (apply-fib-k k 1)]
      [else
       (fib_v3_aux (- n 1)
                   (make-fib-k1 n k))])))

(define fib_v3
  (lambda (v)
    (if (and (integer? v)
             (>= v 0))
        (fib_v3_aux v (make-fib-k0))
        (errorf 'fib_v3
                "not a non-negative integer: ~s"
                v))))

(unless (test-fib fib_v3)
  (printf "(test-fib fib_v3) failed~n"))

;;;;;;;;;;

;;; end of week-11.scm

"week-11.scm"
