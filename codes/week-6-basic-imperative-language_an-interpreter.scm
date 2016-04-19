;;; week-6-basic-imperative-language_an-interpreter.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; The raw Scheme code from
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-6-basic-imperative-language_an-interpreter.html

;;;;;;;;;;

;;; Petite Chez Scheme Version 8.4
;;; Copyright (c) 1985-2011 Cadence Research Systems
;;; 
;;; > (load "week-6-basic-imperative-language_syntax-checker.scm")
;;; > (load "week-6-basic-imperative-language_interpreter.scm")
;;; > (test-run-program run-program)
;;; #t
;;; > 

;;;;;;;;;;

;;; memory footprint: 0
;;; expected result: ()
(define p00
  (make-top
   (make-skip)))

;;; memory footprint: 0
;;; expected result: ()
(define p01
  (make-top
   (make-sequence
    (make-skip)
    (make-skip))))

;;; memory footprint: 1
;;; expected result: (#t)
(define p02
  (make-top
   (make-assign
    (make-location 0)
    (make-constant #t))))

;;; memory footprint: 2
;;; expected result: (100 200)
(define p03
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant 100))
    (make-assign
     (make-location 1)
     (make-constant 200)))))

;;; memory footprint: 4
;;; expected result: (100 200 "uninitialized value" 200)
(define p04
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant 100))
    (make-sequence
     (make-assign
      (make-location 1)
      (make-constant 200))
     (make-assign
      (make-location 3)
      (make-dereference (make-location 1)))))))

;;; memory footprint: 2
;;; expected result: (#t 200)
(define p05a
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant #t))
    (make-conditional
     (make-dereference (make-location 0))
     (make-assign
      (make-location 1)
      (make-constant 200))
     (make-assign
      (make-location 1)
      (make-constant 300))))))

;;; memory footprint: 2
;;; expected result: (#f 300)
(define p05b
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant #f))
    (make-conditional
     (make-dereference (make-location 0))
     (make-assign
      (make-location 1)
      (make-constant 200))
     (make-assign
      (make-location 1)
      (make-constant 300))))))

;;; memory footprint: 2
;;; expected result: (#t #f)
(define p06a
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant #t))
    (make-assign
     (make-location 1)
     (make-binary-operation
      'and
      (make-dereference (make-location 0))
      (make-constant #f))))))

;;; memory footprint: 2
;;; expected result: (#t #t)
(define p06b
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant #t))
    (make-assign
     (make-location 1)
     (make-binary-operation
      'and
      (make-dereference (make-location 0))
      (make-constant #t))))))

;;; memory footprint: 2
;;; expected result: (100 300)
(define p07
  (make-top
   (make-sequence
    (make-assign
     (make-location 0)
     (make-constant 100))
    (make-assign
     (make-location 1)
     (make-binary-operation
      '+
      (make-dereference (make-location 0))
      (make-constant 200))))))

;;; memory footprint: 3
;;; expected result: (n! n+1 n!) for a given n
(define make-factorial-program
  (lambda (n)
    (make-top
     (make-sequence
      (make-assign
       (make-location 0)
       (make-constant n))
      (make-sequence
       (make-assign
        (make-location 1)
        (make-constant 1))
       (make-sequence
        (make-assign
         (make-location 2)
         (make-constant 1))
        (make-sequence
         (make-while
          (make-binary-operation
           '<=
           (make-dereference (make-location 1))
           (make-dereference (make-location 0)))
          (make-sequence
           (make-assign
            (make-location 2)
            (make-binary-operation
             '*
             (make-dereference (make-location 2))
             (make-dereference (make-location 1))))
           (make-assign
            (make-location 1)
            (make-binary-operation
             '+
             (make-dereference (make-location 1))
             (make-constant 1)))))
         (make-assign
          (make-location 0)
          (make-dereference (make-location 2))))))))))

;;;;;;;;;;

(define try-candidate2
  (lambda (candidate input1 input2 verify-output name)
    (or (verify-output (candidate input1 input2))
        (errorf name "error for ~s and ~s" input1 input2))))

(define test-run-program
  (lambda (candidate)
    (and (try-candidate2 candidate
                         p00
                         0
                         null?
                         'p00)
         (try-candidate2 candidate
                         p01
                         0
                         null?
                         'p01)
         (try-candidate2 candidate
                         p02
                         1
                         (lambda (s)
                           (equal? s (list #t)))
                         'p02)
         (try-candidate2 candidate
                         p03
                         2
                         (lambda (s)
                           (equal? s (list 100 200)))
                         'p03)
         (try-candidate2 candidate
                         p04
                         4
                         (lambda (s)
                           (equal? s (list 100 200 "uninitialized value" 200)))
                         'p04)
         (try-candidate2 candidate
                         p05a
                         2
                         (lambda (s)
                           (equal? s (list #t 200)))
                         'p05a)
         (try-candidate2 candidate
                         p05b
                         2
                         (lambda (s)
                           (equal? s (list #f 300)))
                         'p05b)
         (try-candidate2 candidate
                         p06a
                         2
                         (lambda (s)
                           (equal? s (list #t #f)))
                         'p06a)
         (try-candidate2 candidate
                         p06b
                         2
                         (lambda (s)
                           (equal? s (list #t #t)))
                         'p06b)
         (try-candidate2 candidate
                         p07
                         2
                         (lambda (s)
                           (equal? s (list 100 300)))
                         'p07)
         (try-candidate2 candidate
                         (make-factorial-program 0)
                         3
                         (lambda (s)
                           (equal? s (list 1 1 1)))
                         'factorial-program)
         (try-candidate2 candidate
                         (make-factorial-program 1)
                         3
                         (lambda (s)
                           (equal? s (list 1 2 1)))
                         'factorial-program)
         (try-candidate2 candidate
                         (make-factorial-program 5)
                         3
                         (lambda (s)
                           (equal? s (list 120 6 120)))
                         'factorial-program)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; A store is a list of storable values:

(define make-store
  (lambda (v)
    (if (and (integer? v) (>= v 0))
        (make-list v "uninitialized value")
        (errorf 'make-store
                "not a positive integer: ~s"
                v))))

(define make-store-read
  (lambda (n)
    (lambda (s i)
      (if (and (<= 0 i) (< i n))
          (list-ref s i)
          (errorf 'store-read
                  "invalid index: ~s"
                  i)))))

(define make-store-write
  (lambda (n)
    (lambda (s i v)
      (if (and (<= 0 i) (< i n))
          (letrec ([visit (lambda (j s)
                            (if (= j 0)
                                (cons v (cdr s))
                                (cons (car s) (visit (1- j) (cdr s)))))])
            (visit i s))
          (errorf 'store-write
                  "invalid index: ~s"
                  i)))))

;;;;;;;;;;

(define run-unary-operation
  (lambda (op1 v1)
    (case op1
      [(not)
       (if (boolean? v1)
           (not v1)
           (errorf 'run-unary-operation
                   "~s expects a Boolean value: ~s"
                   op1
                   v1))]
      [else
       (errorf 'run-unary-operation
               "unknown operator: ~s"
               op1)])))

(define dispatch-over-binary-operator
  (lambda (op2 kii kbb)
    (case op2
      [(+)
       (kii +)]
      [(-)
       (kii -)]
      [(*)
       (kii *)]
      [(/)
       (kii /)]
      [(<)
       (kii <)]
      [(<=)
       (kii <=)]
      [(=)
       (kii =)]
      [(>=)
       (kii >=)]
      [(>)
       (kii >)]
      [(and)
       (kbb (lambda (v1 v2)
              (and v1 v2)))]
      [(or)
       (kbb (lambda (v1 v2)
              (or v1 v2)))]
      [else
       (errorf 'dispatch-over-binary-operator
               "unknown operator: ~s"
               op2)])))

(define run-binary-operation
  (lambda (op2 v1 v2)
    (dispatch-over-binary-operator op2
                                   (lambda (operator)
                                     (if (and (integer? v1)
                                              (integer? v2))
                                         (operator v1 v2)
                                         (errorf 'run-binary-operation
                                                 "~ expects two integers, not ~s and ~s"
                                                 op2
                                                 v1
                                                 v2)))
                                   (lambda (operator)
                                     (if (and (boolean? v1)
                                              (boolean? v2))
                                         (operator v1 v2)
                                         (errorf 'run-binary-operation
                                                 "~ expects two Booleans, not ~s and ~s"
                                                 op2
                                                 v1
                                                 v2))))))

(define run-expression
  (lambda (e s store-read)
    (letrec ([visit (lambda (e)
                      (cond
                        [(is-constant? e)
                         (constant-1 e)]
                        [(is-dereference? e)
                         (let ([l (dereference-1 e)])
                           (if (is-location? l)
                               (store-read s (location-1 l))
                               (errorf 'run-expression
                                       "not a location: ~s"
                                       l)))]
                        [(is-unary-operation? e)
                         (run-unary-operation (unary-operation-1 e)
                                              (visit (unary-operation-2 e)))]
                        [(is-binary-operation? e)
                         (run-binary-operation (binary-operation-1 e)
                                               (visit (binary-operation-2 e))
                                               (visit (binary-operation-3 e)))]
                        [else
                         (errorf 'run-expression
                                 "not an expression: ~s"
                                 e)]))])
      (visit e))))

(define run-command
  (lambda (c s store-read store-write)
    (letrec ([visit (lambda (c s)
                      (cond
                        [(is-skip? c)
                         s]         
                        [(is-sequence? c)                   
                         (visit (sequence-2 c)
                                (visit (sequence-1 c)
                                       s))]
                        [(is-assign? c)
                         (let ([x (assign-1 c)]
                               [e (assign-2 c)])
                           (if (is-location? x)
                               (store-write s
                                            (dereference-1 x)
                                            (run-expression e s store-read))
                               (errorf 'run-command
                                       "not a reference: ~s"
                                       x)))]
                        [(is-conditional? c)
                         (let ([test (run-expression (conditional-1 c) s store-read)])
                           (if (boolean? test)
                               (if test
                                   (visit (conditional-2 c) s)
                                   (visit (conditional-3 c) s))
                               (errorf 'run-command
                                       "not a boolean value in conditional command: ~s"
                                       test)))] 
                        [(is-while? c)
                         (letrec ([loop (lambda (s)
                                          (let ([test (run-expression (while-1 c) s store-read)])
                                            (if (boolean? test)
                                                (if test
                                                    (loop (visit (while-2 c) s))
                                                    s)
                                                (errorf 'run-command
                                                        "not a boolean value in while loop: ~s"
                                                        test))))])
                           (loop s))] 
                        [(is-switch? c)
                         (errorf 'run-command
                                 "switch not implemented")]
                        [else
                         (errorf 'run-command
                                 "not a command: ~s"
                                 c)]))])
      (visit c s))))

(define run-program
  (lambda (p n)
    (if (is-top? p)
        (run-command (top-1 p)
                     (make-store n)
                     (make-store-read n)
                     (make-store-write n))
        (errorf 'run-program
                "not a program: ~s"
                p))))

;;;;;;;;;;

;;; > (test-run-program run-program)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-6-basic-imperative-language_an-interpreter.scm

"week-6-basic-imperative-language_an-interpreter.scm"
