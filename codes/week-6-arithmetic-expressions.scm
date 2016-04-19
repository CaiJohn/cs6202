;;; week-6-arithmetic-expressions.scm
;;; was:
;;; compilers-arithmetic-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; revised version of 20 Sep 2015
;;; was:
;;; complete version of 18 Sep 2015

;;; Mandatory exercises for Week 6.

;;;;;;;;;;

(load "week-6-arithmetic-expressions-sample.scm")

;;;;;;;;;;

;;; utilities:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (1- n))))))

;;;;;;;;;;

;;; implementation of the BNF for arithmetic expressions:

;;;;;

;;; the constructors:

(define make-literal
  (lambda (n)
    (list 'literal n)))

(define make-plus
  (lambda (e1 e2)
    (list 'plus e1 e2)))

(define make-times
  (lambda (e1 e2)
    (list 'times e1 e2)))

;;; the predicates:

(define is-literal?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'literal)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-plus?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'plus)
         (proper-list-of-given-length? (cdr v) 2))))

(define is-times?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'times)
         (proper-list-of-given-length? (cdr v) 2))))

;;; the accessors:

(define literal-1
  (lambda (v)
    (list-ref v 1)))

(define plus-1
  (lambda (v)
    (list-ref v 1)))

(define plus-2
  (lambda (v)
    (list-ref v 2)))

(define times-1
  (lambda (v)
    (list-ref v 1)))

(define times-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

;;; unparser:

(define unparse-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (list '+
             (unparse-arithmetic-expression (plus-1 e))
             (unparse-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (list '*
             (unparse-arithmetic-expression (times-1 e))
             (unparse-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'unparse-arithmetic-expression
               "unrecognized abstract syntax: ~s"
               e)])))

;;; parser:

(define parse-arithmetic-expression
  (lambda (v)
    (cond
      [(number? v)
       (make-literal v)]
      [(proper-list-of-given-length? v 3)
       (case (list-ref v 0)
         [(+)
          (make-plus (parse-arithmetic-expression (list-ref v 1))
                     (parse-arithmetic-expression (list-ref v 2)))]
         [(*)
          (make-times (parse-arithmetic-expression (list-ref v 1))
                      (parse-arithmetic-expression (list-ref v 2)))]
         [else
          (errorf 'parse-arithmetic-expression
                  "unrecognized operator: ~s"
                  v)])]
      [else
       (errorf 'parse-arithmetic-expression
               "unrecognized concrete syntax: ~s"
               v)])))

;;;;;;;;;;

;;; unit tests:

(define test-interpret-arithmetic-expression
  (lambda (interpret)
    (and (= (interpret (parse-arithmetic-expression source-ae0)) 42)
         (= (interpret (parse-arithmetic-expression source-ae1)) 11)
         (= (interpret (parse-arithmetic-expression source-ae2)) 1111)
         (= (interpret (parse-arithmetic-expression source-ae3)) 120)
         ;;; add more tests here
         )))

(define test-compile-and-run-arithmetic-expression
  (lambda (compile run)
    (and (= (run (compile (parse-arithmetic-expression source-ae0))) 42)
         (= (run (compile (parse-arithmetic-expression source-ae1))) 11)
         (= (run (compile (parse-arithmetic-expression source-ae2))) 1111)
         (= (run (compile (parse-arithmetic-expression source-ae3))) 120)
         ;;; add more tests here
         )))

(define test-interpret-arithmetic-expression_Magritte
  (lambda (interpret_Magritte)
    (and (let ([ae0 (parse-arithmetic-expression source-ae0)])
           (equal? (interpret_Magritte ae0) ae0))
         (let ([ae1 (parse-arithmetic-expression source-ae1)])
           (equal? (interpret_Magritte ae1) ae1))
         (let ([ae2 (parse-arithmetic-expression source-ae2)])
           (equal? (interpret_Magritte ae2) ae2))
         (let ([ae3 (parse-arithmetic-expression source-ae3)])
           (equal? (interpret_Magritte ae3) ae3))
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; interpreter:

(define interpret-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-plus? e)
       (+ (interpret-arithmetic-expression (plus-1 e))
          (interpret-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (* (interpret-arithmetic-expression (times-1 e))
          (interpret-arithmetic-expression (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression interpret-arithmetic-expression)
  (printf "(test-interpret-arithmetic-expression interpret-arithmetic-expression) failed~n"))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code instructions:

;;;;;

;;; the constructors:

(define make-PUSH
  (lambda (n)
    (list 'PUSH n)))

(define make-ADD
  (lambda ()
    (list 'ADD)))

(define make-MUL
  (lambda ()
    (list 'MUL)))

;;; the predicates:

(define is-PUSH?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'PUSH)
         (proper-list-of-given-length? (cdr v) 1))))

(define is-ADD?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'ADD)
         (proper-list-of-given-length? (cdr v) 0))))

(define is-MUL?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'MUL)
         (proper-list-of-given-length? (cdr v) 0))))

;;; the accessors:

(define PUSH-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; implementation of the BNF for the byte-code programs:

(define make-byte-code-program
  (lambda (is)
    (list 'byte-code-program is)))

(define is-byte-code-program?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'byte-code-program)
         (proper-list-of-given-length? (cdr v) 1))))

(define byte-code-program-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; virtual machine:

(define at-least-two?
  (lambda (vs)
    (and (pair? vs)
         (pair? (cdr vs)))))

(define run-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (if (null? is)
                             vs
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is
                                        (cons (PUSH-1 i) vs))]
                                 [(is-ADD? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (+ operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (* operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [else
                                  (errorf 'run-byte-code-program
                                          "unrecognized byte code: ~s"
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

(unless (equal? (run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) 42)
  (printf "(run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD)))) didn't yield 42~n"))

;;;;;;;;;;

;;; compiler:

(define compile-arithmetic-expression
  (lambda (e)
    (letrec ([visit (lambda (e a)
                      (cond
                        [(is-literal? e)
                         (cons (make-PUSH (literal-1 e)) a)]
                        [(is-plus? e)
                         (visit (plus-1 e)
                                (visit (plus-2 e)
                                       (cons (make-ADD) a)))]
                        [(is-times? e)
                         (visit (times-1 e)
                                (visit (times-2 e)
                                       (cons (make-MUL) a)))]
                        [else
                         (errorf 'compile-arithmetic-expression
                                 "unrecognized expression: ~s"
                                 e)]))])
      (make-byte-code-program (visit e '())))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression run-byte-code-program) failed~n"))

;;;;;;;;;;

;;; "just-in-time" compiler:

(define compile-and-run-arithmetic-expression
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression) failed~n"))

;;;;;;;;;;

;;; unit test:

(define does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression ae)))))

(define test_does_interpret-arithmetic-expression_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_diagram_commute?) failed~n"))

;;;;;;;;;;

;;; Magritte virtual machine:

(define run-byte-code-program_Magritte
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is vs)
                         (if (null? is)
                             vs
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is
                                        (cons (make-literal (PUSH-1 i)) vs))]
                                 [(is-ADD? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (make-plus operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [(is-MUL? i)
                                  (if (at-least-two? vs)
                                      (let* ([operand_2 (car vs)]
                                             [vs (cdr vs)]
                                             [operand_1 (car vs)]
                                             [vs (cdr vs)])
                                        (loop is
                                              (cons (make-times operand_1 operand_2)
                                                    vs)))
                                      (errorf 'run-byte-code-program
                                              "stack underflow: ~s"
                                              vs))]
                                 [else
                                  (errorf 'run-byte-code-program
                                          "unrecognized byte code: ~s"
                                          i)]))))])
          (let ([vs (loop (byte-code-program-1 p) '())])
            (if (proper-list-of-given-length? vs 1)
                (car vs)
                (errorf 'run-byte-code-program
                        "unexpected resulting stack: ~s"
                        vs))))
        (errorf 'run-byte-code-program
                "not a byte-code program: ~s"
                p))))

;;; The corresponding "just-in-time" decompiler:

(define compile-and-run-arithmetic-expression_Magritte
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression e))))

(unless (test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte)
  (printf "(test-interpret-arithmetic-expression_Magritte compile-and-run-arithmetic-expression_Magritte) failed~n"))

;;;;;;;;;;

;;; The Magritte interpreter:

(define interpret-arithmetic-expression_Magritte
  (lambda (e)
    (cond
      [(is-literal? e)
       (make-literal (literal-1 e))]
      [(is-plus? e)
       (make-plus (interpret-arithmetic-expression_Magritte (plus-1 e))
                  (interpret-arithmetic-expression_Magritte (plus-2 e)))]
      [(is-times? e)
       (make-times (interpret-arithmetic-expression_Magritte (times-1 e))
                   (interpret-arithmetic-expression_Magritte (times-2 e)))]
      [else
       (errorf 'interpret-arithmetic-expression_Magritte
               "unrecognized expression: ~s"
               e)])))

(unless (test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte)
  (printf "(test-interpret-arithmetic-expression_Magritte interpret-arithmetic-expression_Magritte) failed~n"))

;;; unit test:

(define does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte ae)
              (compile-and-run-arithmetic-expression_Magritte ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_Magritte_make_the_diagram_commute?) failed~n"))

;;;;;;;;;;

;;; Bizarre exercise:

(define compile-arithmetic-expression_bizarre
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (list (make-PUSH (literal-1 e)))]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      (lambda (e2 prepend a2)
                                                        (prepend e2 a2))
                                                      a1)))
                               (list (make-ADD)))]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        (lambda (e2 prepend a2)
                                                          (prepend e2 a2))
                                                        a1)))
                                (list (make-MUL)))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-plus
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (visit-plus (plus-1 e)
                               (lambda (e1 prepend a1)
                                 (prepend e1
                                          (visit-plus (plus-2 e)
                                                      k
                                                      a1)))
                               (cons (make-ADD) a))]
                  [(is-times? e)
                   (k (visit-times (times-1 e)
                                   (lambda (e1 prepend a1)
                                     (prepend e1
                                              (visit-times (times-2 e)
                                                           (lambda (e2 prepend a2)
                                                             (prepend e2 a2))
                                                           a1)))
                                   (list (make-MUL)))
                      append
                      a)]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-times
              (lambda (e k a)
                (cond
                  [(is-literal? e)
                   (k (make-PUSH (literal-1 e)) cons a)]
                  [(is-plus? e)
                   (k (visit-plus (plus-1 e)
                                  (lambda (e1 prepend a1)
                                    (prepend e1
                                             (visit-plus (plus-2 e)
                                                         (lambda (e2 prepend a2)
                                                           (prepend e2 a2))
                                                         a1)))
                                  (list (make-ADD)))
                      append
                      a)]
                  [(is-times? e)
                   (visit-times (times-1 e)
                                (lambda (e1 prepend a1)
                                  (prepend e1
                                           (visit-times (times-2 e)
                                                        k
                                                        a1)))
                                (cons (make-MUL) a))]
                  [else
                   (errorf 'compile-arithmetic-expression_bizarre
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e)))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression_bizarre run-byte-code-program) failed~n"))

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_bizarre
  (lambda (e)
    (run-byte-code-program (compile-arithmetic-expression_bizarre e))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_bizarre) failed~n"))

(define does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_bizarre_diagram_commute?) failed~n"))

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_bizarre e))))

;;; The corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_bizarre
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte_bizarre
            "not implemented yet")))

(define does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_bizarre ae)
              (compile-and-run-arithmetic-expression_Magritte_bizarre ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?)
;;;   (printf "(test_does_interpret-arithmetic-expression_Magritte_bizarre_make_the_diagram_commute?) failed~n"))

;;; In plain English, which bizarre program transformation is performed?

(define is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_bizarre ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_bizarre ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?)
;;;   (printf "(test_is_interpret-arithmetic-expression_Magritte_bizarre_idempotent?) failed~n"))

;;;;;;;;;;

;;; Quaint exercise:

(define compile-arithmetic-expression_quaint
  (lambda (e)
    (letrec ([visit
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (append (visit (plus-1 e) k)
                           (visit (plus-2 e) k)
                           (list (make-ADD)))]
                  [(is-times? e)
                   (visit (times-1 e)
                          (lambda (e1)
                            (visit (times-2 e)
                                   (lambda (e2)
                                     (k (append e1 e2 (list (make-MUL))))))))]
                  [else
                   (errorf 'compile-arithmetic-expression_quaint
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e (lambda (e) e))))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_quaint run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression_quaint run-byte-code-program) failed~n"))

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_quaint
  (lambda (e)
    (run-byte-code-program (compile-arithmetic-expression_quaint e))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_quaint)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_quaint) failed~n"))

(define does_interpret-arithmetic-expression_make_the_quaint_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_quaint ae)))))

(define test_does_interpret-arithmetic-expression_make_the_quaint_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_quaint_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_quaint_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_quaint_diagram_commute?) failed~n"))

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_quaint
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_quaint e))))

;;; The corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_quaint
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte_quaint
            "not implemented yet")))

(define does_interpret-arithmetic-expression_Magritte_quaint_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_quaint ae)
              (compile-and-run-arithmetic-expression_Magritte_quaint ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_quaint_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_quaint_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_quaint_make_the_diagram_commute?)
;;;   (printf "(test_does_interpret-arithmetic-expression_Magritte_quaint_make_the_diagram_commute?) failed~n"))

;;; In plain English, which quaint program transformation is performed?

(define is_interpret-arithmetic-expression_Magritte_quaint_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_quaint ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_quaint ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_quaint_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_quaint_idempotent?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_quaint_idempotent?)
;;;   (printf "(test_is_interpret-arithmetic-expression_Magritte_quaint_idempotent?) failed~n"))

;;;;;;;;;;

;;; Strange exercise:

(define compile-arithmetic-expression_strange
  (lambda (e)
    (letrec ([visit
              (lambda (e)
                (cond
                  [(is-literal? e)
                   (list (make-PUSH (literal-1 e)))]
                  [(is-plus? e)
                   (visit-plus (plus-2 e)
                               (lambda (e2)
                                 (append (visit-plus (plus-1 e)
                                                     append)
                                         e2
                                         (list (make-ADD)))))]
                  [(is-times? e)
                   (visit-times (times-2 e)
                                (lambda (e2)
                                  (append (visit-times (times-1 e)
                                                       append)
                                          e2
                                          (list (make-MUL)))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-plus
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (visit-plus (plus-2 e)
                               (lambda (e2)
                                 (append (visit-plus (plus-1 e)
                                                     k)
                                         e2
                                         (list (make-ADD)))))]
                  [(is-times? e)
                   (k (visit-times (times-2 e)
                                   (lambda (e2)
                                     (append (visit-times (times-1 e)
                                                          append)
                                             e2
                                             (list (make-MUL))))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))]
             [visit-times
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (k (visit-plus (plus-2 e)
                                  (lambda (e2)
                                    (append (visit-plus (plus-1 e)
                                                        append)
                                            e2
                                            (list (make-ADD))))))]
                  [(is-times? e)
                   (visit-times (times-2 e)
                                (lambda (e2)
                                  (append (visit-times (times-1 e)
                                                       k)
                                          e2
                                          (list (make-MUL)))))]
                  [else
                   (errorf 'compile-arithmetic-expression_strange
                           "unrecognized expression: ~s"
                           e)]))])
      (make-byte-code-program (visit e)))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_strange run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression_strange run-byte-code-program) failed~n"))

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_strange
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression_strange ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_strange)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_strange) failed~n"))

(define does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_strange ae)))))

(define test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_strange_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_strange_diagram_commute?) failed~n"))

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_strange
  (lambda (ae)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_strange ae))))

;;; The corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_strange
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte_strange
            "not implemented yet")))

(define does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_strange ae)
              (compile-and-run-arithmetic-expression_Magritte_strange ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?)
;;;   (printf "(test_does_interpret-arithmetic-expression_Magritte_strange_make_the_diagram_commute?) failed~n"))

;;; In plain English, which strange program transformation is performed?

(define is_interpret-arithmetic-expression_Magritte_strange_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_strange ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_strange ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_strange_idempotent?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?)
;;;   (printf "(test_is_interpret-arithmetic-expression_Magritte_strange_idempotent?) failed~n"))

;;;;;;;;;;

;;; Curious exercise:

(define compile-arithmetic-expression_curious
  (lambda (e)
    (letrec ([visit
              (lambda (e k)
                (cond
                  [(is-literal? e)
                   (k (list (make-PUSH (literal-1 e))))]
                  [(is-plus? e)
                   (append (visit (plus-1 e) k)
                           (visit (plus-2 e) k)
                           (list (make-ADD)))]
                  [(is-times? e)
                   (visit (times-2 e)
                          (lambda (bcs2)
                            (visit (times-1 e)
                                   (lambda (bcs1)
                                     (k (append bcs1 bcs2 (list (make-MUL))))))))]
                  [else
                   (errorf 'compile-arithmetic-expression_curious
                           "unrecognized expression: ~s"
                           e)]))])
      (list 'byte-code-program (visit e (lambda (e) e))))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_curious run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression_curious run-byte-code-program) failed~n"))

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_curious
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression_curious ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_curious)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_curious) failed~n"))

(define does_interpret-arithmetic-expression_make_the_curious_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_curious ae)))))

(define test_does_interpret-arithmetic-expression_make_the_curious_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_curious_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_curious_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_curious_diagram_commute?) failed~n"))

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_curious
  (lambda (ae)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_curious ae))))

;;; The corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_curious
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte_curious
            "not implemented yet")))

(define does_interpret-arithmetic-expression_Magritte_curious_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_curious ae)
              (compile-and-run-arithmetic-expression_Magritte_curious ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_curious_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_curious_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_curious_make_the_diagram_commute?)
;;;   (printf "(test_does_interpret-arithmetic-expression_Magritte_curious_make_the_diagram_commute?) failed~n"))

;;; In plain English, which curious program transformation is performed?

(define is_interpret-arithmetic-expression_Magritte_curious_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_curious ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_curious ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_curious_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_curious_idempotent?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_curious_idempotent?)
;;;   (printf "(test_is_interpret-arithmetic-expression_Magritte_curious_idempotent?) failed~n"))

;;;;;;;;;;

;;; Surprising exercise:

(define compile-arithmetic-expression_surprising
  (lambda (e)
    (letrec ([visit
              (lambda (e k0 k1 k)
                (cond
                  [(is-literal? e)
                   (case (literal-1 e)
                     [(0)
                      (k0)]
                     [(1)
                      (k1)]
                     [else
                      (k (list (make-PUSH (literal-1 e))))])]
                  [(is-plus? e)
                   (visit (plus-1 e)
                          (lambda ()
                            (visit (plus-2 e)
                                   k0
                                   k1
                                   k))
                          (lambda ()
                            (visit (plus-2 e)
                                   k1
                                   (lambda ()
                                     (k (list (make-PUSH 1) (make-PUSH 1) (make-ADD))))
                                   (lambda (bcs2)
                                     (k (cons (make-PUSH 1) (append bcs2 (list (make-ADD))))))))
                          (lambda (bcs1)
                            (visit (plus-2 e)
                                   (lambda ()
                                     (k bcs1))
                                   (lambda ()
                                     (k (append bcs1 (list (make-PUSH 1) (make-ADD)))))
                                   (lambda (bcs2)
                                     (k (append bcs1 bcs2 (list (make-ADD))))))))]
                  [(is-times? e)
                   (visit (times-1 e)
                          k0
                          (lambda ()
                            (visit (times-2 e)
                                   k0
                                   k1
                                   k))
                          (lambda (bcs1)
                            (visit (times-2 e)
                                   k0
                                   (lambda ()
                                     (k bcs1))
                                   (lambda (bcs2)
                                     (k (append bcs1 bcs2 (list (make-MUL))))))))]
                  [else
                   (errorf 'compile-arithmetic-expression_surprising
                           "unrecognized expression: ~s"
                           e)]))])
      (list 'byte-code-program
            (visit e
                   (lambda ()
                     (list (make-PUSH 0)))
                   (lambda ()
                     (list (make-PUSH 1)))
                   (lambda (bcs)
                     bcs))))))

(unless (test-compile-and-run-arithmetic-expression compile-arithmetic-expression_surprising run-byte-code-program)
  (printf "(test-compile-and-run-arithmetic-expression compile-arithmetic-expression_surprising run-byte-code-program) failed~n"))

;;; The corresponding "just-in-time" optimizing compiler:

(define compile-and-run-arithmetic-expression_surprising
  (lambda (ae)
    (run-byte-code-program (compile-arithmetic-expression_surprising ae))))

(unless (test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_curious)
  (printf "(test-interpret-arithmetic-expression compile-and-run-arithmetic-expression_curious) failed~n"))

(define does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression ae)
              (compile-and-run-arithmetic-expression_surprising ae)))))

(define test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?
            sample-of-arithmetic-expressions)))

(unless (test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?)
  (printf "(test_does_interpret-arithmetic-expression_make_the_surprising_diagram_commute?) failed~n"))

;;; The corresponding "just-in-time" optimizing decompiler:

(define compile-and-run-arithmetic-expression_Magritte_surprising
  (lambda (e)
    (run-byte-code-program_Magritte (compile-arithmetic-expression_surprising e))))

;;; The corresponding optimizing Magritte interpreter:

(define interpret-arithmetic-expression_Magritte_surprising
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte_surprising
            "not implemented yet")))

(define does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
  (lambda (source-ae)
    (let ([ae (parse-arithmetic-expression source-ae)])
      (equal? (interpret-arithmetic-expression_Magritte_surprising ae)
              (compile-and-run-arithmetic-expression_Magritte_surprising ae)))))

(define test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
  (lambda ()
    (andmap does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?)
;;;   (printf "(test_does_interpret-arithmetic-expression_Magritte_surprising_make_the_diagram_commute?) failed~n"))

;;; In plain English, which surprising program transformation is performed?

(define is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
  (lambda (source-ae)
    (let* ([ae (parse-arithmetic-expression source-ae)]
           [ae_optimized (interpret-arithmetic-expression_Magritte_surprising ae)])
      (equal? ae_optimized
              (interpret-arithmetic-expression_Magritte_surprising ae_optimized)))))

(define test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
  (lambda ()
    (andmap is_interpret-arithmetic-expression_Magritte_surprising_idempotent?
            sample-of-arithmetic-expressions)))

;;; Uncomment the following lines to test your implementation when loading this file:
;;; (unless (test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?)
;;;   (printf "(test_is_interpret-arithmetic-expression_Magritte_surprising_idempotent?) failed~n"))

;;;;;;;;;;

;;; end of week-6-arithmetic-expressions.scm

"week-6-arithmetic-expressions.scm"
