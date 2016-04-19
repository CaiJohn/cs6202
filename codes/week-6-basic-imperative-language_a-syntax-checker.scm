;;; week-6-basic-imperative-language_a-syntax-checker.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; The raw Scheme code from
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-6-basic-imperative-language_a-syntax-checker.html

;;;;;;;;;;

(define check-literal
  (lambda (v)
    (errorf 'check-literal "not implemented yet")))

(define check-unary-operator
  (lambda (v)
    (errorf 'check-unary-operator "not implemented yet")))

(define check-binary-operator
  (lambda (v)
    (errorf 'check-binary-operator "not implemented yet")))

(define check-reference
  (lambda (v)
    (errorf 'check-reference "not implemented yet")))

(define check-expression
  (lambda (v)
    (errorf 'check-expression "not implemented yet")))

(define check-command
  (lambda (v)
    (errorf 'check-command "not implemented yet")))

(define check-program
  (lambda (v)
    (errorf 'check-program "not implemented yet")))

;;;;;

(define trace-all-compound-predicates
  (lambda ()
    (trace check-literal
           check-unary-operator
           check-binary-operator
           check-reference
           check-expression
           check-command
           check-program)))

(define untrace-all-compound-predicates
  (lambda ()
    (untrace check-literal
             check-unary-operator
             check-binary-operator
             check-reference
             check-expression
             check-command
             check-program)))

;;;;;;;;;;

;;; constructors:

;;; <reference>

(define make-location
  (lambda (natural-number)
    (list 'location natural-number)))

;;; <expression>

(define make-constant
  (lambda (literal)
    (list 'constant literal)))

(define make-dereference
  (lambda (reference)
    (list 'dereference reference)))

(define make-unary-operation
  (lambda (unary-operator expression)
    (list 'unary-operation unary-operator expression)))

(define make-binary-operation
  (lambda (binary-operator expression_1 expression_2)
    (list 'binary-operation binary-operator expression_1 expression_2)))

;;; <command>

(define make-skip
  (lambda ()
    (list 'skip)))

(define make-sequence
  (lambda (command_1 command_2)
    (list 'sequence command_1 command_2)))

(define make-assign
  (lambda (reference expression)
    (list 'assign reference expression)))

(define make-conditional
  (lambda (expression command_1 command_2)
    (list 'conditional expression command_1 command_2)))

(define make-while
  (lambda (expression command)
    (list 'while expression command)))

(define make-switch
  (lambda (expression switch-clauses command)
    (append (list 'switch expression)
            switch-clauses
            (list (list 'otherwise command)))))

;;; <program>

(define make-top
  (lambda (command)
    (list 'top command)))

;;;;;;;;;;

(define sample-of-well-formed-literals
  (list ;;; Booleans:
        #f 
        ;;; Integers:
        0
        ;;; add more here
        ))

(define sample-of-well-formed-references
  (list ;;;
        (make-location 0)
        ;;; add more here
        ))

(define sample-of-well-formed-expressions
  (list ;;;
        (make-constant #t)
        ;;; add more here
        ))

(define sample-of-well-formed-commands
  (list ;;;
        (make-skip)
        ;;; add more here
        ))

(define sample-of-well-formed-programs
  (list ;;;
        (make-top (make-skip))
        ;;; add more here
        ))

;;;;;;;;;;

;;; predicates:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

(define list-at-least-as-long-as?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (or (= i 0)
                          (and (pair? v)
                               (visit (cdr v)
                                      (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          #f))))

;;; <reference>

(define is-location?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'location))))

;;; <expression>

(define is-constant?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'constant))))

(define is-dereference?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'dereference))))

(define is-unary-operation?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'unary-operation))))

(define is-binary-operation?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'binary-operation))))

;;; <command>

(define is-skip?
  (lambda (v)
    (and (proper-list-of-given-length? v 1)
         (equal? (car v) 'skip))))

(define is-sequence?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'sequence))))

(define is-assign?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'assign))))

(define is-conditional?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'conditional))))

(define is-while?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'while))))

(define is-switch?
  (lambda (v)
    (and (list-at-least-as-long-as? v 3)
         (equal? (car v) 'switch))))

;;; <program>

(define is-top?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'top))))

;;;;;;;;;;

;;; accessors:

;;; <reference>

(define location-1
  (lambda (v)
    (list-ref v 1)))

;;; <expression>

(define literal-1
  (lambda (v)
    (list-ref v 1)))

(define constant-1
  (lambda (v)
    (list-ref v 1)))

(define dereference-1
  (lambda (v)
    (list-ref v 1)))

(define unary-operation-1
  (lambda (v)
    (list-ref v 1)))

(define unary-operation-2
  (lambda (v)
    (list-ref v 2)))

(define binary-operation-1
  (lambda (v)
    (list-ref v 1)))

(define binary-operation-2
  (lambda (v)
    (list-ref v 2)))

(define binary-operation-3
  (lambda (v)
    (list-ref v 3)))

;;; <command>

(define sequence-1
  (lambda (v)
    (list-ref v 1)))

(define sequence-2
  (lambda (v)
    (list-ref v 2)))

(define assign-1
  (lambda (v)
    (list-ref v 1)))

(define assign-2
  (lambda (v)
    (list-ref v 2)))

(define conditional-1
  (lambda (v)
    (list-ref v 1)))

(define conditional-2
  (lambda (v)
    (list-ref v 2)))

(define conditional-3
  (lambda (v)
    (list-ref v 3)))

(define while-1
  (lambda (v)
    (list-ref v 1)))

(define while-2
  (lambda (v)
    (list-ref v 2)))

(define switch-1
  (lambda (v)
    (list-ref v 1)))

(define switch-rest
  (lambda (v)
    (list-tail v 2)))

;;; <program>

(define top-1
  (lambda (v)
    (list-ref v 1)))

;;;;;

(define trace-all-ground-predicates
  (lambda ()
    (trace is-location?
           is-constant?
           is-dereference?
           is-unary-operation?
           is-binary-operation?
           is-skip?
           is-sequence?
           is-assign?
           is-conditional?
           is-while?
           is-switch?
           is-top?)))

(define untrace-all-ground-predicates
  (lambda ()
    (untrace is-location?
             is-constant?
             is-dereference?
             is-unary-operation?
             is-binary-operation?
             is-skip?
             is-sequence?
             is-assign?
             is-conditional?
             is-while?
             is-switch?
             is-top?)))

;;;;;;;;;;

(define test-well-formed-things
  (lambda (name is-thing? things)
    (letrec ([visit (lambda (things)
                      (cond
                        [(null? things)
                         #t]
                        [(pair? things)
                         (if (is-thing? (car things))
                             (visit (cdr things))
                             (errorf 'test-well-formed-things
                                     "incorrect result for ~s ~s"
                                     name
                                     (car things)))]
                        [else
                         (errorf 'test-well-formed-things
                                 "not a proper list: ~s"
                                 things)]))])
      (visit things))))

(define test-well-formed-literals
  (lambda ()
    (test-well-formed-things 'literal
                             check-literal
                             sample-of-well-formed-literals)))

(define test-well-formed-references
  (lambda ()
    (test-well-formed-things 'reference
                             check-reference
                             sample-of-well-formed-references)))

(define test-well-formed-expressions
  (lambda ()
    (test-well-formed-things 'expression
                             check-expression
                             sample-of-well-formed-expressions)))

(define test-well-formed-commands
  (lambda ()
    (test-well-formed-things 'command
                             check-command
                             sample-of-well-formed-commands)))

(define test-well-formed-programs
  (lambda ()
    (test-well-formed-things 'program
                             check-program
                             sample-of-well-formed-programs)))

(define test-all-well-formed-things
  (lambda ()
    (and (test-well-formed-literals)
         (test-well-formed-references)
         (test-well-formed-expressions)
         (test-well-formed-commands)
         (test-well-formed-programs))))

;;;;;;;;;;

(define check-silently
  #t)

;;;;;;;;;;

(define check-literal
  (lambda (v)
    (cond
      [(boolean? v)
       #t]
      [(integer? v)
       #t]
      [else
       (begin
         (or check-silently
             (printf "check-literal -- ill-formed input: ~s~n" v))
         #f)])))

;;; > (test-well-formed-literals)
;;; #t
;;; > 

(define sample-of-well-formed-literals
  (append (list ;;; Booleans:
                #t
                ;;; Integers:
                1
                -1
                2
                -2
                ;;; add more here
                )
          sample-of-well-formed-literals))

;;; > (test-well-formed-literals)
;;; #t
;;; > 

;;;;;;;;;;

(define check-reference
  (lambda (v)
    (or (and (is-location? v)
             (integer? (location-1 v))
             (>= (location-1 v) 0))
        (begin
          (or check-silently
              (printf "check-reference -- ill-formed input: ~s~n" v))
          #f))))

;;; > (test-well-formed-references)
;;; #t
;;; > 

(define sample-of-well-formed-references
  (append (list ;;;
                (make-location 10)
                (make-location 100)
                (make-location 1000)
                ;;; add more here
                )
           sample-of-well-formed-references))

;;; > (test-well-formed-references)
;;; #t
;;; > 

;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
      [(is-constant? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-dereference? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-unary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-binary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [else
       #f])))

(define sample-of-well-formed-expressions
  (cons (make-constant 42)
        sample-of-well-formed-expressions))

;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
      [(is-constant? v)                 ;;; <---***---
       (check-literal (constant-1 v))]  ;;; <---***---
      [(is-dereference? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-unary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-binary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

(define make-random-constant
  (lambda ()
    (make-constant (list-ref sample-of-well-formed-literals
                             (random (length sample-of-well-formed-literals))))))

(define sample-of-well-formed-expressions
  (cons (make-random-constant)
        (cons (make-random-constant)
              (cons (make-random-constant)
                    sample-of-well-formed-expressions))))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-well-formed-expressions
  (cons (make-dereference (make-location 0))
        sample-of-well-formed-expressions))

(define check-expression
  (lambda (v)
    (cond
      [(is-constant? v)
       (check-literal (constant-1 v))]
      [(is-dereference? v)                 ;;; <---***---
       (check-reference (dereference-1 v))]  ;;; <---***---
      [(is-unary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [(is-binary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define make-random-dereference
  (lambda ()
    (make-dereference (list-ref sample-of-well-formed-references
                                (random (length sample-of-well-formed-references))))))

(define sample-of-well-formed-expressions
  (cons (make-random-dereference)
        (cons (make-random-dereference)
              (cons (make-random-dereference)
                    sample-of-well-formed-expressions))))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define unary-operators
  '(not))
  
(define check-unary-operator
  (lambda (v)
    (or (member v unary-operators)
        (begin
          (or check-silently
              (printf "check-unary-operator -- ill-formed input: ~s~n" v))
          #f))))

(define sample-of-well-formed-expressions
  (cons (make-unary-operation 'not
                              (make-constant #t))
        sample-of-well-formed-expressions))

;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
      [(is-constant? v)
       (check-literal (constant-1 v))]
      [(is-dereference? v)
       (check-reference (dereference-1 v))]
      [(is-unary-operation? v)                            ;;; <---***---
       (and (check-unary-operator (unary-operation-1 v))  ;;; <---***---
            (check-expression (unary-operation-2 v)))]    ;;; <---***---
      [(is-binary-operation? v)
       (errorf 'check-expression "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define make-random-unary-operation
  (lambda ()
    (let ([number-of-unary-operators (length unary-operators)]
          [number-of-well-formed-expressions (length sample-of-well-formed-expressions)])
      (make-unary-operation (list-ref unary-operators
                                      (random number-of-unary-operators))
                            (list-ref sample-of-well-formed-expressions
                                      (random number-of-well-formed-expressions))))))

(define sample-of-well-formed-expressions
  (cons (make-random-unary-operation)
        sample-of-well-formed-expressions))

(define sample-of-well-formed-expressions
  (cons (make-random-unary-operation)
        sample-of-well-formed-expressions))

(define sample-of-well-formed-expressions
  (cons (make-random-unary-operation)
        sample-of-well-formed-expressions))

;;; ad libitum

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define binary-operators
  '(+ - * / < <= = > >= and or))

(define check-binary-operator
  (lambda (v)
    (or (member v binary-operators)
        (begin
          (or check-silently
              (printf "check-binary-operator -- ill-formed input: ~s~n" v))
          #f))))

(define sample-of-well-formed-expressions
  (cons (make-binary-operation '+
                               (make-constant 10)
                               (make-constant 100))
        (cons (make-binary-operation 'and
                                     (make-constant #t)
                                     (make-constant #f))
              sample-of-well-formed-expressions)))

(define check-expression
  (lambda (v)
    (cond
      [(is-constant? v)
       (check-literal (constant-1 v))]
      [(is-dereference? v)
       (check-reference (dereference-1 v))]
      [(is-unary-operation? v)
       (and (check-unary-operator (unary-operation-1 v))
            (check-expression (unary-operation-2 v)))]
      [(is-binary-operation? v)                             ;;; <---***---
       (and (check-binary-operator (binary-operation-1 v))  ;;; <---***---
            (check-expression (binary-operation-2 v))       ;;; <---***---
            (check-expression (binary-operation-3 v)))]     ;;; <---***---
      [else
       #f])))

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define make-random-binary-operation
  (lambda ()
    (let ([number-of-binary-operators (length binary-operators)]
          [number-of-well-formed-expressions (length sample-of-well-formed-expressions)])
      (make-binary-operation (list-ref binary-operators
                                       (random number-of-binary-operators))
                             (list-ref sample-of-well-formed-expressions
                                       (random number-of-well-formed-expressions))
                             (list-ref sample-of-well-formed-expressions
                                       (random number-of-well-formed-expressions))))))

(define sample-of-well-formed-expressions
  (cons (make-random-binary-operation)
        sample-of-well-formed-expressions))

(define sample-of-well-formed-expressions
  (cons (make-random-binary-operation)
        sample-of-well-formed-expressions))

(define sample-of-well-formed-expressions
  (cons (make-random-binary-operation)
        sample-of-well-formed-expressions))

;;; ad libitum

;;; > (test-well-formed-expressions)
;;; #t
;;; > 

;;;;;;;;;;

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-sequence? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-assign? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-conditional? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-while? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;;;;;;;;;

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)  ;;; <---***---
       #t]           ;;; <---***---
      [(is-sequence? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-assign? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-conditional? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-while? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

(define sample-of-well-formed-commands
  (cons (make-sequence (make-skip)
                       (make-skip))
        sample-of-well-formed-commands))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       #t]         
      [(is-sequence? v)                       ;;; <---***---
       (and (check-command (sequence-1 v))    ;;; <---***---
            (check-command (sequence-2 v)))]  ;;; <---***---
      [(is-assign? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-conditional? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-while? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

(define make-random-sequence
  (lambda ()
    (make-sequence (list-ref sample-of-well-formed-commands
                             (random (length sample-of-well-formed-commands)))
                   (list-ref sample-of-well-formed-commands
                             (random (length sample-of-well-formed-commands))))))

(define sample-of-well-formed-commands
  (cons (make-random-sequence)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-sequence)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-sequence)
        sample-of-well-formed-commands))

;;; ad libitum

;;; (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-well-formed-commands
  (cons (make-assign (make-location 0)
                     (make-constant #t))
        sample-of-well-formed-commands))

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       #t]         
      [(is-sequence? v)                   
       (and (check-command (sequence-1 v))  
            (check-command (sequence-2 v)))]
      [(is-assign? v)                          ;;; <---***---
       (and (check-reference (assign-1 v))     ;;; <---***---
            (check-expression (assign-2 v)))]  ;;; <---***---
      [(is-conditional? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-while? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;; (test-well-formed-commands)
;;; #t
;;; > 

(define make-random-assignment
  (lambda ()
    (make-assign (list-ref sample-of-well-formed-references
                           (random (length sample-of-well-formed-references)))
                 (list-ref sample-of-well-formed-expressions
                           (random (length sample-of-well-formed-expressions))))))

(define sample-of-well-formed-commands
  (cons (make-random-assignment)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-assignment)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-assignment)
        sample-of-well-formed-commands))

;;; ad libitum

;;; > (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-well-formed-commands
  (cons (make-conditional (make-constant #t)
                          (make-skip)
                          (make-skip))
        sample-of-well-formed-commands))

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       #t]         
      [(is-sequence? v)                   
       (and (check-command (sequence-1 v))  
            (check-command (sequence-2 v)))]
      [(is-assign? v)                      
       (and (check-reference (assign-1 v))   
            (check-expression (assign-2 v)))]
      [(is-conditional? v)                        ;;; <---***---
       (and (check-expression (conditional-1 v))  ;;; <---***---
            (check-command (conditional-2 v))     ;;; <---***---
            (check-command (conditional-3 v)))]   ;;; <---***---
      [(is-while? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

(define make-random-conditional-command
  (lambda ()
    (let ([number-of-well-formed-commands (length sample-of-well-formed-commands)])
      (make-conditional (list-ref sample-of-well-formed-expressions
                                  (random (length sample-of-well-formed-expressions)))
                        (list-ref sample-of-well-formed-commands
                                  (random number-of-well-formed-commands))
                        (list-ref sample-of-well-formed-commands
                                  (random number-of-well-formed-commands))))))

(define sample-of-well-formed-commands
  (cons (make-random-conditional-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-conditional-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-conditional-command)
        sample-of-well-formed-commands))

;;; ad libitum

;;; > (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-well-formed-commands
  (cons (make-while (make-constant #f)
                    (make-skip))
        sample-of-well-formed-commands))

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       #t]         
      [(is-sequence? v)                   
       (and (check-command (sequence-1 v))  
            (check-command (sequence-2 v)))]
      [(is-assign? v)                      
       (and (check-reference (assign-1 v))   
            (check-expression (assign-2 v)))]
      [(is-conditional? v)                    
       (and (check-expression (conditional-1 v))
            (check-command (conditional-2 v))   
            (check-command (conditional-3 v)))] 
      [(is-while? v)                        ;;; <---***---
       (and (check-expression (while-1 v))  ;;; <---***---
            (check-command (while-2 v)))]   ;;; <---***---
      [(is-switch? v)
       (errorf 'check-command "not implemented yet: ~s" v)]
      [else
       #f])))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

(define make-random-while-command
  (lambda ()
    (make-while (list-ref sample-of-well-formed-expressions
                          (random (length sample-of-well-formed-expressions)))
                (list-ref sample-of-well-formed-commands
                          (random (length sample-of-well-formed-commands))))))

(define sample-of-well-formed-commands
  (cons (make-random-while-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-while-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-while-command)
        sample-of-well-formed-commands))

;;; ad libitum

;;; > (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-well-formed-commands
  (cons (make-switch (make-constant #f)
                     '()
                     (make-skip))
        (cons (make-switch (make-constant #f)
                           (list (list #f (make-skip)))
                           (make-skip))
              sample-of-well-formed-commands)))

(define check-command
  (lambda (v)
    (cond
      [(is-skip? v)
       #t]         
      [(is-sequence? v)                   
       (and (check-command (sequence-1 v))  
            (check-command (sequence-2 v)))]
      [(is-assign? v)                      
       (and (check-reference (assign-1 v))   
            (check-expression (assign-2 v)))]
      [(is-conditional? v)                    
       (and (check-expression (conditional-1 v))
            (check-command (conditional-2 v))   
            (check-command (conditional-3 v)))] 
      [(is-while? v)                    
       (and (check-expression (while-1 v))
            (check-command (while-2 v)))] 
      [(is-switch? v)
       (is-switch-command? (switch-1 v)
                           (switch-rest v))]
      [else
       #f])))

(define is-switch-command?
  (lambda (v vs)
    (and (check-expression v)
         (letrec ([visit (lambda (purported-clause rest)
                           (cond
                             [(null? rest)
                              (and (proper-list-of-given-length? purported-clause 2)
                                   (equal? (car purported-clause) 'otherwise)
                                   (check-command (cadr purported-clause)))]
                             [(pair? rest)
                              (and (proper-list-of-given-length? purported-clause 2)
                                   (check-literal (car purported-clause))
                                   (check-command (cadr purported-clause))
                                   (visit (car rest) (cdr rest)))]
                             [else
                              #f]))])
           (visit (car vs) (cdr vs))))))

;;; > (test-well-formed-commands)
;;; #t
;;; > 

(define maximum-number-of-clauses-in-random-switch-command
  10)

(define make-random-switch-command
  (lambda ()
    (make-switch (list-ref sample-of-well-formed-expressions
                          (random (length sample-of-well-formed-expressions)))
                 (map (lambda (whatever)
                        (list (list-ref sample-of-well-formed-literals
                                        (random (length sample-of-well-formed-literals)))
                              (list-ref sample-of-well-formed-commands
                                        (random (length sample-of-well-formed-commands)))))
                      (iota (random maximum-number-of-clauses-in-random-switch-command)))
                 (list-ref sample-of-well-formed-commands
                           (random (length sample-of-well-formed-commands))))))

(define sample-of-well-formed-commands
  (cons (make-random-switch-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-switch-command)
        sample-of-well-formed-commands))

(define sample-of-well-formed-commands
  (cons (make-random-switch-command)
        sample-of-well-formed-commands))

;;; ad libitum

;;; > (test-well-formed-commands)
;;; #t
;;; > 

;;;;;;;;;;

(define check-program
  (lambda (v)
    (if (is-top? v)
        (check-command (top-1 v))
        (begin
          (or check-silently
              (printf "check-program -- ill-formed input: ~s~n" v))
          #f))))

;;; > (test-well-formed-programs)
;;; #t
;;; > 

(define make-random-program
  (lambda ()
    (make-top (list-ref sample-of-well-formed-commands
                        (random (length sample-of-well-formed-commands))))))

(define sample-of-well-formed-programs
  (cons (make-random-program)
        sample-of-well-formed-programs))

(define sample-of-well-formed-programs
  (cons (make-random-program)
        sample-of-well-formed-programs))

(define sample-of-well-formed-programs
  (cons (make-random-program)
        sample-of-well-formed-programs))

;;; ad libitum

;;; > (test-well-formed-programs)
;;; #t
;;; > 

;;;;;;;;;;

;;; > (test-all-well-formed-things)
;;; #t
;;; > 

;;;;;;;;;;

(define sample-of-ill-formed-literals
  (list '()
        "foo"
        'bar
        ;;; etc.
        ))

(define sample-of-ill-formed-references
  (list '()
        'location
        '(location)
        '(location . whatever)
        '(location -1)
        '(location 0 . whatever)
        '(location 0 whatever)
        ;;; etc.
        ))

(define sample-of-ill-formed-expressions
  (list '()
        ;;;
        '(dereference)
        '(dereference . whatever)
        '(dereference 0)
        '(dereference (make-location 0) whatever)
        '(dereference (make-location 0) . whatever)
        ;;; etc.
        ;;;
        '(constant)
        '(constant . whatever)
        '(constant "foo")
        '(constant 42 whatever)
        '(constant 42 . whatever)
        ;;; etc.
        ;;; 
        '(unary-operation)
        '(unary-operation . whatever)
        '(unary-operation 'not)
        '(unary-operation 'not . whatever)
        '(unary-operation 'not (make-constant #f) . whatever)
        '(unary-operation 'not (make-constant #f) whatever)
        ;;; etc.
        ;;;
        '(binary-operation)
        '(binary-operation . whatever)
        '(binary-operation +)
        '(binary-operation + . whatever)
        '(binary-operation + (constant 10))
        '(binary-operation + (constant 10) . whatever)
        '(binary-operation + (constant 10) (constant 100) . whatever)
        '(binary-operation + (constant 10) (constant 100) whatever)
        ;;; etc.
        ))

(define sample-of-ill-formed-commands
  (list '()
        ;;;
        'skip
        '(skip . whatever)
        '(skip whatever)
        ;;;
        '(sequence)
        '(sequence . whatever)
        '(sequence (skip)
                   . whatever)
        '(sequence (skip)
                   (assign 1 (constant 1001))
                   . whatever)
        '(sequence (skip)
                   (assign 1 (constant 1001))
                   whatever)
        ;;; etc.
        ;;;
        '(assign)
        '(assign . whatever)
        '(assign 0)
        '(assign 0 . whatever)
        '(assign 0 (constant 1000) . whatever)
        '(assign 0 (constant 1000) whatever)
        ;;; etc.
        ;;;
        '(conditional)
        '(conditional . whatever)
        '(conditional (constant #t)
                      . whatever)
        '(conditional (constant #t)
                      (skip))
        '(conditional (constant #t)
                      (skip)
                      . whatever)
        '(conditional (constant #t)
                      (skip)
                      (assign 1 (constant 1001))
                      . whatever)
        '(conditional (constant #t)
                      (skip)
                      (assign 1 (constant 1001))
                      whatever)
        ;;; etc.
        ;;;
        '(while)
        '(while . whatever)
        '(while (constant #f)
                . whatever)
        '(while (constant #f)
                (skip)
                . whatever)
        '(while (constant #f)
                (skip)
                whatever)
        ;;; etc.
        ;;;
        '(switch)
        '(switch . whatever)
        '(switch (constant #f)
           . whatever)
        '(switch (constant #f)
           whatever)
        '(switch (constant #f)
           [whatever otherwise])
        '(switch (constant #f)
           [otherwise (skip)]
           . whatever)
        '(switch (constant #f)
            [otherwise (skip)]
            whatever)
        '(switch (constant #f)
           whatever
           [otherwise (skip)])
        '(switch (constant #f)
           [whatever]
           [otherwise (skip)])
        '(switch (constant #f)
           [whatever-1 . whatever-2]
           [otherwise (skip)])
        '(switch (constant #f)
           [whatever-1 whatever-2 . whatever-3]
           [otherwise (skip)])
        '(switch (constant #f)
           [whatever-1 whatever-2 whatever-3]
           [otherwise (skip)])
        '(switch (constant #f)
           [#t (constant 1000)]
           [otherwise (skip)])
        ;;; etc.
        ))

(define sample-of-ill-formed-programs
  (list '()
        'top
        '(top)
        '(top . whatever)
        '(top (skip) . whatever)
        '(top (skip) whatever)
        ;;; etc.
        ))

;;;;;;;;;;

(define test-ill-formed-things
    (lambda (name is-thing? things)
      (letrec ([visit (lambda (things)
                        (cond
                          [(null? things)
                           #t]
                          [(pair? things)
                           (if (is-thing? (car things))
                               (errorf 'test-ill-formed-things
                                       "incorrect result for ~s ~s"
                                       name
                                       (car things))
                               (visit (cdr things)))]
                          [else
                           (errorf 'test-ill-formed-things
                                   "not a proper list: ~s"
                                   things)]))])
        (visit things))))

;;;;;;;;;;

(define test-ill-formed-literals
  (lambda ()
    (test-ill-formed-things 'literal
                            check-literal
                            sample-of-ill-formed-literals)))

(define test-ill-formed-references
  (lambda ()
    (test-ill-formed-things 'reference
                            check-reference
                            sample-of-ill-formed-references)))

(define test-ill-formed-expressions
  (lambda ()
    (test-ill-formed-things 'expression
                            check-expression
                            sample-of-ill-formed-expressions)))

(define test-ill-formed-commands
  (lambda ()
    (test-ill-formed-things 'command
                            check-command
                            sample-of-ill-formed-commands)))

(define test-ill-formed-programs
  (lambda ()
    (test-ill-formed-things 'program
                            check-program
                            sample-of-ill-formed-programs)))

;;;;;;;;;;

(define test-all-ill-formed-things
  (lambda ()
    (and (test-ill-formed-literals)
         (test-ill-formed-references)
         (test-ill-formed-expressions)
         (test-ill-formed-commands)
         (test-ill-formed-programs))))

;;; > (test-all-ill-formed-things)
;;; #t
;;; > 

;;;;;;;;;;

(define test-all
  (lambda ()
    (and (test-all-well-formed-things)
         (test-all-ill-formed-things))))

;;; > (test-all)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-6-basic-imperative-language_a-syntax-checker.scm

"week-6-basic-imperative-language_a-syntax-checker.scm"
