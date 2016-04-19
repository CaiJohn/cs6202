;;; week-4-arithmetic-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 01 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-4-arithmetic-expressions.html

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

;;; sample of well-formed arithmetic expressions:

(define ae0
  (make-literal 42))

(define ae1
  (make-plus (make-literal 1)
             (make-literal 10)))

(define ae2
  (make-plus ae1
             (make-plus (make-literal 100)
                        (make-literal 1000))))

(define ae3
  (make-times
    (make-times
      (make-times
        (make-literal 1)
        (make-literal 2))
      (make-literal 3))
    (make-times
      (make-literal 4)
      (make-literal 5))))

;;; unit tests:

(define test-well-formed-arithmetic-expressions
  (lambda (check)
    (and (check ae0)
         (check ae1)
         (check ae2)
         (check ae3)
         ;;; add more tests here
         )))

(define test-interpret-arithmetic-expressions
  (lambda (interpret)
    (and (= (interpret ae0) 42)
         (= (interpret ae1) 11)
         (= (interpret ae2) 1111)
         (= (interpret ae3) 120)
         ;;; add more tests here
         )))

(define test-compile-arithmetic-expressions
  (lambda (compile check)
    (and (check (compile ae0))
         (check (compile ae1))
         (check (compile ae2))
         (check (compile ae3))
         ;;; add more tests here
         )))

(define test-compile-and-run-arithmetic-expressions
  (lambda (compile run)
    (and (= (run (compile ae0)) 42)
         (= (run (compile ae1)) 11)
         (= (run (compile ae2)) 1111)
         (= (run (compile ae3)) 120)
         ;;; add more tests here
         )))

;;;;;;;;;;

;;; sample of ill-formed arithmetic expressions:

(define ea0
  42)

(define ea1
  '(literal))

(define ea2
  '(literal . whatever))

(define ea3
  '(literal 10 . whatever))

(define ea4
  '(literal 10 20))

(define ea5
  '(plus))

(define ea6
  '(plus . whatever))

(define ea7
  '(plus (literal 10) . whatever))

(define ea8
  '(plus (literal 10) (literal 20) . whatever))

(define ea9
  '(plus (literal 10) (literal 20) (literal 30)))

(define test-ill-formed-arithmetic-expressions
  (lambda (check)
    (not (or (check ea0)
             (check ea1)
             (check ea2)
             (check ea3)
             (check ea4)
             (check ea5)
             (check ea6)
             (check ea7)
             (check ea8)
             (check ea9)
             ;;; add more tests here
             ))))

;;;;;;;;;;

;;; syntax checker:

(define check-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (number? (literal-1 e))]
      [(is-plus? e)
       (and (check-arithmetic-expression (plus-1 e))
            (check-arithmetic-expression (plus-2 e)))]
      [(is-times? e)
       (and (check-arithmetic-expression (times-1 e))
            (check-arithmetic-expression (times-2 e)))]
      [else
       #f])))

;;; > (test-well-formed-arithmetic-expressions check-arithmetic-expression)
;;; #t
;;; > (test-ill-formed-arithmetic-expressions check-arithmetic-expression)
;;; #t
;;; > 

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

;;; > (test-interpret-arithmetic-expressions interpret-arithmetic-expression)
;;; #t
;;; > 

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

;;; sample of well-formed byte-code programs:

(define p0
  (make-byte-code-program '((PUSH 42))))

(define p1
  (make-byte-code-program '((PUSH 20) (PUSH 22) (ADD))))

(define q0
  (make-byte-code-program '((PUSH 1) (PUSH 2))))

(define q1
  (make-byte-code-program '((PUSH 1) (ADD))))

;;; unit test:

(define test-well-formed-byte-code-programs
  (lambda (check)
    (and (check p0)
         (check p1)
         (check q0)
         (check q1)
         ;;; add more tests here
         )))

(define test-run-byte-code-programs
  (lambda (run)
    (and (= (run p0) 42)
         (= (run p1) 42)
         ;;; add more tests here
         )))

;;; sample of ill-formed byte-code programs:

(define z0
  '())

(define z1
  (make-byte-code-program '((PUSH 42) . whatever)))

(define z2
  (make-byte-code-program '((ADD 42))))

(define z3
  (make-byte-code-program '((PUSH "42"))))

(define test-ill-formed-byte-code-programs
  (lambda (check)
    (not (or (check z0)
             (check z1)
             (check z2)
             (check z3)
             ;;; add more tests here
             ))))

;;;;;;;;;;

;;; syntax checker:

(define check-byte-code-instruction
  (lambda (v)
    (cond
      [(is-PUSH? v)
       (number? (PUSH-1 v))]
      [(is-ADD? v)
       #t]
      [(is-MUL? v)
       #t]
      [else
       #f])))

(define check-byte-code-program
  (lambda (v)
    (if (is-byte-code-program? v)
        (letrec ([loop (lambda (v)
                         (cond
                           [(null? v)
                            #t]
                           [(pair? v)
                            (and (check-byte-code-instruction (car v))
                                 (loop (cdr v)))]
                           [else
                            #f]))])
          (loop (byte-code-program-1 v)))
        #f)))

;;; > (test-compile-arithmetic-expressions compile-arithmetic-expression check-byte-code-program)
;;; #t
;;; > 

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

;;; > (test-compile-arithmetic-expressions compile-arithmetic-expression check-byte-code-program)
;;; #t
;;; > (run-byte-code-program '(byte-code-program ((PUSH 20) (PUSH 22) (ADD))))
;;; 42
;;; > 

;;;;;;;;;;

;;; compiler:

(define compile-arithmetic-expression
  (lambda (e)
    (letrec ([visit (lambda (e)
                      (cond
                        [(is-literal? e)
                         (list (make-PUSH (literal-1 e)))]
                        [(is-plus? e)
                         (append (visit (plus-1 e))
                                 (visit (plus-2 e))
                                 (list (make-ADD)))]
                        [(is-times? e)
                         (append (visit (times-1 e))
                                 (visit (times-2 e))
                                 (list (make-MUL)))]
                        [else
                         (errorf 'compile-arithmetic-expression
                                 "unrecognized expression: ~s"
                                 e)]))])
      (make-byte-code-program (visit e)))))

;;; > (test-compile-arithmetic-expressions compile-arithmetic-expression check-byte-code-program)
;;; #t
;;; > (test-compile-and-run-arithmetic-expressions compile-arithmetic-expression run-byte-code-program)
;;; #t
;;; > 

;;;;;;;;;;

;;; well-behaved byte-code programs:

(define p0
  (make-byte-code-program '((PUSH 42))))

(define p1
  (make-byte-code-program '((PUSH 20) (PUSH 22) (ADD))))

(define verify-well-behaved-byte-code-programs
  (lambda (verify)
    (and (verify p0)
         (verify p1)
         ;;; add more tests here
         )))

;;; ill-behaved byte-code programs:

(define q0
  (make-byte-code-program '((PUSH 1) (PUSH 2))))

(define q1
  (make-byte-code-program '((PUSH 1) (ADD))))

(define verify-ill-behaved-byte-code-programs
  (lambda (verify)
    (not (and (verify q0)
              (verify q1)
              ;;; add more tests here
              ))))

;;; output of the compiler:

(define test-compiled-arithmetic-expressions
  (lambda (compile verify)
    (and (verify (compile ae0))
         (verify (compile ae1))
         (verify (compile ae2))
         (verify (compile ae3))
         ;;; add more tests here
         )))

;;; Byte-code verifier:

(define verify-byte-code-program
  (lambda (p)
    (if (is-byte-code-program? p)
        (letrec ([loop (lambda (is n)
                         (if (null? is)
                             n
                             (let ([i (car is)]
                                   [is (cdr is)])
                               (cond
                                 [(is-PUSH? i)
                                  (loop is (1+ n))]
                                 [(is-ADD? i)
                                  (and (>= n 2)
                                       (loop is (1- n)))]
                                 [(is-MUL? i)
                                  (and (>= n 2)
                                       (loop is (1- n)))]
                                 [else
                                  (errorf 'verify-byte-code-program
                                          "unrecognized byte code: ~s"
                                          i)]))))])
          (let ([result (loop (byte-code-program-1 p) 0)])
            (and result
                 (= result 1))))
        (errorf 'verify-byte-code-program
                "not a byte-code program: ~s"
                p))))

;;; > (verify-well-behaved-byte-code-programs verify-byte-code-program)
;;; #t
;;; > (verify-ill-behaved-byte-code-programs verify-byte-code-program)
;;; #t
;; > (test-compiled-arithmetic-expressions compile-arithmetic-expression verify-byte-code-program)
;; #t
;; > 

;;;;;;;;;;

(define run-verified-byte-code-program
  (lambda (p)
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
                              (let* ([operand_2 (car vs)]
                                     [vs (cdr vs)]
                                     [operand_1 (car vs)]
                                     [vs (cdr vs)])
                                (loop is
                                      (cons (+ operand_1 operand_2)
                                            vs)))]
                             [else
                              (let* ([operand_2 (car vs)]
                                     [vs (cdr vs)]
                                     [operand_1 (car vs)]
                                     [vs (cdr vs)])
                                (loop is
                                      (cons (* operand_1 operand_2)
                                            vs)))]))))])
      (car (loop (byte-code-program-1 p) '())))))

;;; > (test-compile-and-run-arithmetic-expressions compile-arithmetic-expression run-verified-byte-code-program)
;;; #t
;;; > (run-verified-byte-code-program q0)
;;; 2
;;; > (run-verified-byte-code-program q1)
;;; 
;;; Exception in car: () is not a pair
;;; Type (debug) to enter the debugger.
;;; > 

(define p100000
  (make-byte-code-program
    (append
      (make-list 100000 '(PUSH 1))
      (make-list 99999 '(ADD)))))

;;; > (verify-byte-code-program p100000)
;;; #t
;;; > (time (run-byte-code-program p100000))
;;; (time (run-byte-code-program p100000))
;;;     5 collections
;;;     337 ms elapsed cpu time, including 12 ms collecting
;;;     355 ms elapsed real time, including 12 ms collecting
;;;     22400520 bytes allocated, including 20420504 bytes reclaimed
;;; 100000
;;; > (time (run-verified-byte-code-program p100000))
;;; (time (run-verified-byte-code-program p100000))
;;;     5 collections
;;;     295 ms elapsed cpu time, including 14 ms collecting
;;;     346 ms elapsed real time, including 15 ms collecting
;;;     22400512 bytes allocated, including 21538440 bytes reclaimed
;;; 100000
;;; > 

;;;;;;;;;;

;;; interactive comparison:

(define compile-and-run-arithmetic-expression
  (lambda (e)
    (run-byte-code-program
       (compile-arithmetic-expression
         e))))

(define test-commutation-for-arithmetic-expressions
  (lambda (e)
    (let ([result-1 (interpret-arithmetic-expression e)]
          [result-2 (compile-and-run-arithmetic-expression e)])
      (list result-1 result-2 (equal? result-1 result-2)))))

;;;;;;;;;;

;;; Magritte virtual machine:

(define run-byte-code-program_Magritte
  (lambda (p)
    (errorf 'run-byte-code-program_Magritte
            "not implemented yet")))

(define test-Magritte
  (lambda (e)
    (list "This is not an arithmetic expression."
          e
          (run-byte-code-program_Magritte (compile-arithmetic-expression e)))))

;;;;;;;;;;

;;; Magritte interpreter:

(define compile-and-run-arithmetic-expression_Magritte
  (lambda (e)
    (run-byte-code-program_Magritte
     (compile-arithmetic-expression
      e))))

(define test-commutation-for-arithmetic-expressions_Magritte
  (lambda (e)
    (let ([result-1 (interpret-arithmetic-expression_Magritte e)]
          [result-2 (compile-and-run-arithmetic-expression_Magritte e)])
      (list result-1 result-2 (equal? result-1 result-2)))))

(define interpret-arithmetic-expression_Magritte
  (lambda (e)
    (errorf 'interpret-arithmetic-expression_Magritte
            "not implemented yet")))

;;;;;;;;;;

;;; end of week-4-arithmetic-expressions.scm

"week-4-arithmetic-expressions.scm"
