;;; week-12-arithmetic-expressions.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 13 Nov 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-12.html

;;;;;;;;;;;;;;;;;;;;

;;; utility:

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (1- n))))))

;;;;;;;;;;

;;; source language:

;;; n ::= ...any Scheme integer...
;;; t ::= (literal n) | (minus t t) | (divide t t)

;;; constructors:

(define make-literal
  (lambda (n)
    (list 'literal n)))

(define make-minus
  (lambda (e1 e2)
    (list 'minus e1 e2)))

(define make-divide
  (lambda (e1 e2)
    (list 'divide e1 e2)))

;;; predicates:

(define is-literal?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'literal))))

(define is-minus?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'minus))))

(define is-divide?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'divide))))

;;; the accessors:

(define literal-1
  (lambda (v)
    (list-ref v 1)))

(define minus-1
  (lambda (v)
    (list-ref v 1)))

(define minus-2
  (lambda (v)
    (list-ref v 2)))

(define divide-1
  (lambda (v)
    (list-ref v 1)))

(define divide-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

;;; unparser:

(define unparse-arithmetic-expression
  (lambda (e)
    (cond
      [(is-literal? e)
       (literal-1 e)]
      [(is-minus? e)
       (list '-
             (unparse-arithmetic-expression (plus-1 e))
             (unparse-arithmetic-expression (plus-2 e)))]
      [(is-divide? e)
       (list '/
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
         [(-)
          (make-minus (parse-arithmetic-expression (list-ref v 1))
                     (parse-arithmetic-expression (list-ref v 2)))]
         [(/)
          (make-divide (parse-arithmetic-expression (list-ref v 1))
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

;;; language of normal forms:

;;; n ::= ...any Scheme integer...
;;; t_nf ::= (literal_nf n)

;;; constructor:

(define make-literal_nf
  (lambda (n)
    (list 'literal_nf n)))

;;; predicate:

(define is-literal_nf?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'literal_nf))))

;;; the accessors:

(define literal_nf-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

(define embed-t_nf-to-t
  (lambda (t_nf)
    (cond
      [(is-literal_nf? t_nf)
       (make-literal (literal_nf-1 t_nf))]
      [else
       (errorf 'embed-t_nf-to-t
               "not a nf: ~s"
               t_nnf)])))

;;;;;;;;;;

;;; <potential-redex> ::= (pr_minus t t) | (pr_divide t t)

;;; constructors:

(define make-pr_minus
  (lambda (t1 t2)
    (list 'pr_minus t1 t2)))

(define make-pr_divide
  (lambda (t1 t2)
    (list 'pr_divide t1 t2)))

;;; predicates:

(define is-pr_minus?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'pr_minus))))

(define is-pr_divide?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'pr_divide))))

;;; accessors:

(define pr_minus-1
  (lambda (v)
    (list-ref v 1)))

(define pr_minus-2
  (lambda (v)
    (list-ref v 2)))

(define pr_divide-1
  (lambda (v)
    (list-ref v 1)))

(define pr_divide-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

(define potential-redex-to-term
  (lambda (pr)
    (cond
      [(is-pr_minus? pr)
       (make-minus (make-literal (pr_minus-1 pr)) (make-literal (pr_minus-2 pr)))]
      [(is-pr_divide? pr)
       (make-divide (make-literal (pr_divide-1 pr)) (make-literal (pr_divide-2 pr)))]
      [else
       (errorf 'potential-redex-to-term
               "not a potential redex: ~s"
               pr)])))

;;;;;;;;;;

;;; contractum_or_error ::= (contractum t) | (irreducible <string>)

;;; constructors:

(define make-contractum
  (lambda (t)
    (list 'contractum t)))

(define make-irreducible
  (lambda (t)
    (list 'irreducible t)))

;;; predicates:

(define is-contractum?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'contractum))))

(define is-irreducible?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'irreducible))))

;;; accessors:

(define contractum-1
  (lambda (v)
    (list-ref v 1)))

(define irreducible-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

(define contract
  (lambda (pr)
    (cond
      [(is-pr_minus? pr)
       (make-contractum (make-literal (- (literal_nf-1 (pr_minus-1 pr))
                                         (literal_nf-1 (pr_minus-2 pr)))))]
      [(is-pr_divide? pr)
       (if (= (literal_nf-1 (pr_minus-2 pr)) 0)
           (make-irreducible (format "division by 0: ~s" (pr_minus-1 pr)))
           (make-contractum (make-literal (quotient (literal_nf-1 (pr_minus-1 pr))
                                                    (literal_nf-1 (pr_minus-2 pr))))))]
      [else
       (errorf 'contract
               "not a potential redex: ~s"
               pr)])))

;;;;;;;;;;

;;; result-of-reduction ::= (value t_nf) | (term t) | (stuck <string>)

;;; constructors

(define make-value
  (lambda (t_nf)
    (list 'value t_nf)))

(define make-term
  (lambda (t)
    (list 'term t)))

(define make-stuck
  (lambda (s)
    (list 'stuck s)))

;;; predicates:

(define is-value?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'value))))

(define is-term?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'term))))

(define is-stuck?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'stuck))))

;;; accessors:

(define value-1
  (lambda (v)
    (list-ref v 1)))

(define term-1
  (lambda (v)
    (list-ref v 1)))

(define stuck-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; leftmost-innermost one-step reduction:

(define reduce
  (lambda (t)
    (reduce-term t)))

(define reduce-term
  (lambda (t)
    (cond
      [(is-literal? t)
       (reduce-term-literal (literal-1 t))]
      [(is-minus? t)
       (reduce-term-minus (minus-1 t) (minus-2 t))]
      [(is-divide? t)
       (reduce-term-divide (divide-1 t) (divide-2 t))]
      [else
       (errorf 'reduce-term
               "ill-formed term: ~s"
               t)])))

(define reduce-term-literal
  (lambda (n)
    (make-value (make-literal_nf n))))

(define reduce-term-minus
  (lambda (t1 t2)
    (let ([result-of-reduction-1 (reduce-term t1)])
      (cond
        [(is-value? result-of-reduction-1)
         (let ([result-of-reduction-2 (reduce-term t2)])
           (cond
             [(is-value? result-of-reduction-2)
              (let ([c-or-i (contract (make-pr_minus (value-1 result-of-reduction-1) (value-1 result-of-reduction-2)))])
                (cond
                  [(is-contractum? c-or-i)
                   (make-term (contractum-1 c-or-i))]
                  [(is-irreducible? c-or-i)
                   (make-stuck (irreducible-1 c-or-i))]
                  [else
                   (errorf 'reduce-term-minus
                           "wrong intermediate result of contract: ~s"
                           c-or-i)]))]
             [(is-term? result-of-reduction-2)
              (make-term (make-minus (embed-t_nf-to-t (value-1 result-of-reduction-1))
                                     (term-1 result-of-reduction-2)))]
             [(is-stuck? result-of-reduction-2)
              result-of-reduction-2]
             [else
              (errorf 'reduce-term-minus
                      "unexpected intermediate result of reduce-term: ~s"
                      result-of-reduction-2)]))]
        [(is-term? result-of-reduction-1)
         (make-term (make-minus (term-1 result-of-reduction-1) t2))]
        [(is-stuck? result-of-reduction-1)
         result-of-reduction-1]
        [else
         (errorf 'reduce-term-minus
                 "unexpected intermediate result of reduce-term: ~s"
                 result-of-reduction-1)]))))

(define reduce-term-divide
  (lambda (t1 t2)
    (let ([result-of-reduction-1 (reduce-term t1)])
      (cond
        [(is-value? result-of-reduction-1)
         (let ([result-of-reduction-2 (reduce-term t2)])
           (cond
             [(is-value? result-of-reduction-2)
              (let ([c-or-i (contract (make-pr_divide (value-1 result-of-reduction-1) (value-1 result-of-reduction-2)))])
                (cond
                  [(is-contractum? c-or-i)
                   (make-term (contractum-1 c-or-i))]
                  [(is-irreducible? c-or-i)
                   (make-stuck (irreducible-1 c-or-i))]
                  [else
                   (errorf 'reduce-term-divide
                           "wrong intermediate result of contract: ~s"
                           c-or-i)]))]
             [(is-term? result-of-reduction-2)
              (make-term (make-divide (embed-t_nf-to-t (value-1 result-of-reduction-1))
                                      (term-1 result-of-reduction-2)))]
             [(is-stuck? result-of-reduction-2)
              result-of-reduction-2]
             [else
              (errorf 'reduce-term-divide
                      "unexpected intermediate result of reduce-term: ~s"
                      result-of-reduction-2)]))]
        [(is-term? result-of-reduction-1)
         (make-term (make-divide (term-1 result-of-reduction-1) t2))]
        [(is-stuck? result-of-reduction-1)
         result-of-reduction-1]
        [else
         (errorf 'reduce-term-divide
                 "unexpected intermediate result of reduce-term: ~s"
                 result-of-reduction-1)]))))

;;;;;;;;;;

;;; result-of-evaluation ::= (nf t_nf) | (wrong <string>)

;;; constructors

(define make-nf
  (lambda (t_nf)
    (list 'nf t_nf)))

(define make-wrong
  (lambda (s)
    (list 'wrong s)))

;;; predicates:

(define is-nf?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'nf))))

(define is-wrong?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'wrong))))

;;; accessors:

(define nf-1
  (lambda (v)
    (list-ref v 1)))

(define wrong-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; reduction-based evaluation:

(define evaluate
  (lambda (t)
    (let ([r (reduce t)])
      (cond
        [(is-value? r)
         (make-nf (value-1 r))]
        [(is-term? r)
         (evaluate (term-1 r))]
        [(is-stuck? r)
         (make-wrong (stuck-1 r))]
        [else
         (errorf 'evaluate
                 "unexpected intermediate result of reduce: ~s"
                 r)]))))

;;;;;;;;;;

;;; unit tests:

(define and-all
  (lambda bs
    (andmap (lambda (b) b) bs)))

(define verify-normalize-arithmetic-expression
  (lambda (candidate)
    (lambda (t a)
      (or (equal? (candidate t) a)
          (begin
            (printf "failed: ~s~n" t)
            #f)))))

(define test-normalize-arithmetic-expression
  (lambda (candidate)
    (let ([verify-candidate (verify-normalize-arithmetic-expression candidate)])
      (and-all (verify-candidate (parse-arithmetic-expression '42)
                                 (make-nf (make-literal_nf '42)))
               (verify-candidate (parse-arithmetic-expression '(- 10 -5))
                                 (make-nf (make-literal_nf '15)))
               (verify-candidate (parse-arithmetic-expression '(- (- 1000 499) (- 800 400)))
                                 (make-nf (make-literal_nf '101)))
               (verify-candidate (parse-arithmetic-expression '(/ 1000 100))
                                 (make-nf (make-literal_nf '10)))
               (verify-candidate (parse-arithmetic-expression '(/ (/ 10000 10) (/ 100 10)))
                                 (make-nf (make-literal_nf '100)))
               (verify-candidate (parse-arithmetic-expression '(/ (/ (/ 10000 10) (/ 100 10)) 10))
                                 (make-nf (make-literal_nf '10)))
               (verify-candidate (parse-arithmetic-expression '(/ (/ (/ (- 50000 40000) 10) (/ 100 10)) 10))
                                 (make-nf (make-literal_nf '10)))
               (verify-candidate (parse-arithmetic-expression '(/ 10 0))
                                 (make-wrong "division by 0: (literal_nf 10)"))
               (verify-candidate (parse-arithmetic-expression '(- (- (/ 10 0) 0) (/ 100 0)))
                                 (make-wrong "division by 0: (literal_nf 10)"))
               ;;; etc.
               'done))))

(unless (test-normalize-arithmetic-expression evaluate)
  (printf "(test-normalize-arithmetic-expression normalize-arithmetic-expression) failed~n"))

;;;;;;;;;;

;;; end of week-12-arithmetic-expressions.scm

"week-12-arithmetic-expressions.scm"
