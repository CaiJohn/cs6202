;;; week-12-boolean-expressions.scm
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

;;; x ::= ...any Scheme symbol...
;;; t ::= (var x) | (neg t) | (conj t t) | (disj t t)

;;; constructors:

(define make-var
  (lambda (x)
    (list 'var x)))

(define make-neg
  (lambda (f)
    (list 'neg f)))

(define make-conj
  (lambda (f1 f2)
    (list 'conj f1 f2)))

(define make-disj
  (lambda (f1 f2)
    (list 'disj f1 f2)))

;;; predicates:

(define is-var?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'var))))

(define is-neg?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'neg))))

(define is-conj?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'conj))))

(define is-disj?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'disj))))

;;; accessors:

(define var-1
  (lambda (v)
    (list-ref v 1)))

(define neg-1
  (lambda (v)
    (list-ref v 1)))

(define conj-1
  (lambda (v)
    (list-ref v 1)))

(define conj-2
  (lambda (v)
    (list-ref v 2)))

(define disj-1
  (lambda (v)
    (list-ref v 1)))

(define disj-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

;;; language of normal forms:

;;; x ::= ...any Scheme symbol...
;;; lit ::= (posvar x) | (negvar x)
;;; t_nnf ::= (lit_nnf lit) | (conj_nnf t_nnf t_nnf) | (disj_nnf t_nnf t_nnf)

;;; constructors:

(define make-posvar
  (lambda (x)
    (list 'posvar x)))

(define make-negvar
  (lambda (x)
    (list 'negvar x)))

;;; predicates:

(define is-posvar?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'posvar))))

(define is-negvar?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'negvar))))

;;; accessors:

(define posvar-1
  (lambda (v)
    (list-ref v 1)))

(define negvar-1
  (lambda (v)
    (list-ref v 1)))

;;; constructors:

(define make-lit_nnf
  (lambda (e)
    (list 'lit_nnf e)))

(define make-conj_nnf
  (lambda (e1 e2)
    (list 'conj_nnf e1 e2)))

(define make-disj_nnf
  (lambda (e1 e2)
    (list 'disj_nnf e1 e2)))

;;; predicates:

(define is-lit_nnf?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'lit_nnf))))

(define is-conj_nnf?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'conj_nnf))))

(define is-disj_nnf?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'disj_nnf))))

;;; accessors:

(define lit_nnf-1
  (lambda (v)
    (list-ref v 1)))

(define conj_nnf-1
  (lambda (v)
    (list-ref v 1)))

(define conj_nnf-2
  (lambda (v)
    (list-ref v 2)))

(define disj_nnf-1
  (lambda (v)
    (list-ref v 1)))

(define disj_nnf-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

(define embed-lit_nnf-to-t
  (lambda (lit_nnf)
    (cond
      [(is-posvar? lit_nnf)
       (make-var (posvar-1 lit_nnf))]
      [(is-negvar? lit_nnf)
       (make-neg (make-var (negvar-1 lit_nnf)))]
      [else
       (errorf 'embed-lit_nnf-to-t
               "not a lit_nnf: ~s"
               lit_nnf)])))

(define embed-t_nnf-to-t
  (lambda (t_nnf)
    (cond
      [(is-lit_nnf? t_nnf)
       (embed-lit_nnf-to-t (lit_nnf-1 t_nnf))]
      [(is-conj_nnf? t_nnf)
       (make-conj (embed-t_nnf-to-t (conj_nnf-1 t_nnf))
                  (embed-t_nnf-to-t (conj_nnf-2 t_nnf)))]
      [(is-disj_nnf? t_nnf)
       (make-disj (embed-t_nnf-to-t (disj_nnf-1 t_nnf))
                  (embed-t_nnf-to-t (disj_nnf-2 t_nnf)))]
      [else
       (errorf 'embed-t_nnf-to-t
               "not an nnf: ~s"
               t_nnf)])))

;;;;;;;;;;

;;; <potential-redex> ::= (pr_neg-neg t) | (pr_neg-conj t t) | (pr_neg-disj t t)

;;; constructors:

(define make-pr_neg-neg
  (lambda (t)
    (list 'pr_neg-neg t)))

(define make-pr_neg-conj
  (lambda (t1 t2)
    (list 'pr_neg-conj t1 t2)))

(define make-pr_neg-disj
  (lambda (t1 t2)
    (list 'pr_neg-disj t1 t2)))

;;; predicates:

(define is-pr_neg-neg?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'pr_neg-neg))))

(define is-pr_neg-conj?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'pr_neg-conj))))

(define is-pr_neg-disj?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'pr_neg-disj))))

;;; accessors:

(define pr_neg-neg-1
  (lambda (v)
    (list-ref v 1)))

(define pr_neg-conj-1
  (lambda (v)
    (list-ref v 1)))

(define pr_neg-conj-2
  (lambda (v)
    (list-ref v 2)))

(define pr_neg-disj-1
  (lambda (v)
    (list-ref v 1)))

(define pr_neg-disj-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;

(define potential-redex-to-term
  (lambda (pr)
    (cond
      [(is-pr_neg-neg? pr)
       (make-neg (make-neg (pr_neg-neg-1 pr)))]
      [(is-pr_neg-conj? pr)
       (make-neg (make-conj (pr_neg-conj-1 pr) (pr_neg-conj-2 pr)))]
      [(is-pr_neg-disj? pr)
       (make-neg (make-disj (pr_neg-disj-1 pr) (pr_neg-disj-2 pr)))]
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
      [(is-pr_neg-neg? pr)
       (make-contractum (pr_neg-neg-1 pr))]
      [(is-pr_neg-conj? pr)
       (make-contractum (make-disj (make-neg (pr_neg-conj-1 pr))
                                   (make-neg (pr_neg-conj-2 pr))))]
      [(is-pr_neg-disj? pr)
       (make-contractum (make-conj (make-neg (pr_neg-disj-1 pr))
                                   (make-neg (pr_neg-disj-2 pr))))]
      [else
       (errorf 'contract
               "not a potential redex: ~s"
               pr)])))

;;;;;;;;;;

;;; result-of-reduction ::= (value t_nnf) | (term t) | (stuck <string>)

;;; constructors

(define make-value
  (lambda (t_nnf)
    (list 'value t_nnf)))

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

;;; leftmost-outermost one-step reduction:

(define reduce
  (lambda (t)
    (reduce-term t)))

(define reduce-term
  (lambda (t)
    (cond
      [(is-var? t)
       (reduce-term-var (var-1 t))]
      [(is-neg? t)
       (reduce-term-neg (neg-1 t))]
      [(is-conj? t)
       (reduce-term-conj (conj-1 t) (conj-2 t))]
      [(is-disj? t)
       (reduce-term-disj (disj-1 t) (disj-2 t))]
      [else
       (errorf 'reduce-term
               "ill-formed term: ~s"
               t)])))

(define reduce-term-var
  (lambda (x)
    (make-value (make-lit_nnf (make-posvar x)))))

(define reduce-term-neg
  (lambda (t)
    (cond
      [(is-var? t)
       (make-value (make-lit_nnf (make-negvar (var-1 t))))]
      [(is-neg? t)
       (let ([c-or-i (contract (make-pr_neg-neg (neg-1 t)))])
         (cond
           [(is-contractum? c-or-i)
            (make-term (contractum-1 c-or-i))]
           [(is-irreducible? c-or-i)
            (make-stuck (irreducible-1 c-or-i))]
           [else
            (errorf 'reduce-term-neg
                    "unexpected intermediate result of contract: ~s"
                    c-or-i)]))]
      [(is-conj? t)
       (let ([c-or-i (contract (make-pr_neg-conj (conj-1 t) (conj-2 t)))])
         (cond
           [(is-contractum? c-or-i)
            (make-term (contractum-1 c-or-i))]
           [(is-irreducible? c-or-i)
            (make-stuck (irreducible-1 c-or-i))]
           [else
            (errorf 'reduce-term-neg
                    "unexpected intermediate result of contract: ~s"
                    c-or-i)]))]
      [(is-disj? t)
       (let ([c-or-i (contract (make-pr_neg-disj (disj-1 t) (disj-2 t)))])
         (cond
           [(is-contractum? c-or-i)
            (make-term (contractum-1 c-or-i))]
           [(is-irreducible? c-or-i)
            (make-stuck (irreducible-1 c-or-i))]
           [else
            (errorf 'reduce-term-neg
                    "unexpected intermediate result of contract: ~s"
                    c-or-i)]))]
      [else
       (errorf 'reduce-term-neg
               "ill-formed term: ~s"
               t)])))

(define reduce-term-conj
  (lambda (t1 t2)
    (let ([z1 (reduce-term t1)])
      (cond
        [(is-value? z1)
         (let ([z2 (reduce-term t2)])
           (cond
             [(is-value? z2)
              (make-value (make-conj_nnf (value-1 z1) (value-1 z2)))]
             [(is-term? z2)
              (make-term (make-conj (embed-t_nnf-to-t (value-1 z1)) (term-1 z2)))]
             [(is-error? z2)
              z2]
             [else
              (errorf 'reduce-term-conj
                      "unexpected intermediate result of reduce-term: ~s"
                      z2)]))]
        [(is-term? z1)
         (make-term (make-conj (term-1 z1) t2))]
        [else
         (errorf 'reduce-term-conj
                 "unexpected intermediate result of reduce-term: ~s"
                 z1)]))))

(define reduce-term-disj
  (lambda (t1 t2)
    (let ([z1 (reduce-term t1)])
      (cond
        [(is-value? z1)
         (let ([z2 (reduce-term t2)])
           (cond
             [(is-value? z2)
              (make-value (make-disj_nnf (value-1 z1) (value-1 z2)))]
             [(is-term? z2)
              (make-term (make-disj (embed-t_nnf-to-t (value-1 z1)) (term-1 z2)))]
             [(is-error? z2)
              z2]
             [else
              (errorf 'reduce-term-disj
                      "unexpected intermediate result of reduce-term: ~s"
                      z2)]))]
        [(is-term? z1)
         (make-term (make-disj (term-1 z1) t2))]
        [else
         (errorf 'reduce-term-disj
                 "unexpected intermediate result of reduce-term: ~s"
                 z1)]))))

;;;;;;;;;;

;;; result-of-evaluation ::= (nnf t_nnf) | (wrong <string>)

;;; constructors

(define make-nnf
  (lambda (t_nnf)
    (list 'nnf t_nnf)))

(define make-wrong
  (lambda (s)
    (list 'wrong s)))

;;; predicates:

(define is-nnf?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'nnf))))

(define is-wrong?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'wrong))))

;;; accessors:

(define nnf-1
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
         (make-nnf (value-1 r))]
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

(define verify-normalize-boolean-expression
  (lambda (candidate)
    (lambda (t result)
      (or (equal? (candidate t) result)
          (begin
            (printf "failed: ~s~n" t)
            #f)))))

(define test-normalize-boolean-expression
  (lambda (candidate)
    (let ([verify-candidate (verify-normalize-boolean-expression candidate)])
      (and-all (verify-candidate '(var x)
                                 (make-nnf '(lit_nnf (posvar x))))
               (verify-candidate '(conj (var x)
                                        (var y))
                                 (make-nnf '(conj_nnf (lit_nnf (posvar x))
                                                      (lit_nnf (posvar y)))))
               (verify-candidate '(disj (var x)
                                        (var y))
                                 (make-nnf '(disj_nnf (lit_nnf (posvar x))
                                                      (lit_nnf (posvar y)))))
               (verify-candidate '(conj (disj (var a) (var b)) (disj (var c) (var d)))
                                 (make-nnf '(conj_nnf (disj_nnf (lit_nnf (posvar a))
                                                                (lit_nnf (posvar b)))
                                                      (disj_nnf (lit_nnf (posvar c))
                                                                (lit_nnf (posvar d))))))
               (verify-candidate '(conj (disj (neg (var a))
                                              (neg (var b)))
                                        (disj (neg (var c))
                                              (neg (var d))))
                                 (make-nnf '(conj_nnf (disj_nnf (lit_nnf (negvar a))
                                                                (lit_nnf (negvar b)))
                                                      (disj_nnf (lit_nnf (negvar c))
                                                                (lit_nnf (negvar d))))))
               (verify-candidate '(neg (var x))
                                 (make-nnf '(lit_nnf (negvar x))))
               (verify-candidate '(neg (neg (neg (neg (var x)))))
                                 (make-nnf '(lit_nnf (posvar x))))
               (verify-candidate '(neg (neg (neg (neg (neg (var x))))))
                                 (make-nnf '(lit_nnf (negvar x))))
               (verify-candidate '(neg (conj (var x) (neg (var y))))
                                 (make-nnf '(disj_nnf (lit_nnf (negvar x)) (lit_nnf (posvar y)))))
               (verify-candidate '(neg (conj (neg (var x)) (var y)))
                                 (make-nnf '(disj_nnf (lit_nnf (posvar x)) (lit_nnf (negvar y)))))
               (verify-candidate '(neg (disj (var x) (neg (var y))))
                                 (make-nnf '(conj_nnf (lit_nnf (negvar x)) (lit_nnf (posvar y)))))
               (verify-candidate '(neg (disj (neg (var x)) (var y)))
                                 (make-nnf '(conj_nnf (lit_nnf (posvar x)) (lit_nnf (negvar y)))))
               (verify-candidate '(conj (disj (var x)
                                              (neg (var y)))
                                        (neg (conj (var z)
                                                   (var x))))
                                 (make-nnf '(conj_nnf (disj_nnf (lit_nnf (posvar x))
                                                                (lit_nnf (negvar y)))
                                                      (disj_nnf (lit_nnf (negvar z))
                                                                (lit_nnf (negvar x))))))
               ;;; etc.
               'done))))

(unless (test-normalize-boolean-expression evaluate)
  (printf "(test-normalize-boolean-expression normalize-boolean-expression) failed~n"))

;;;;;;;;;;

;;; end of week-12-boolean-expressions.scm

"week-12-boolean-expressions.scm"
