;;; week-3-environments.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 25 Aug 2015

;;; Accompanying material for the lecture notes at
;;;   http://users-cs.au.dk/danvy/dProgSprog15/Lecture-notes/week-2-environments.html

;;;;;;;;;;

(define test-env
  (lambda (mt extend lookup)
    (and ;;; first test:
     (or (lookup 'x
                 mt
                 (lambda (denotable)
                   #f)
                 (lambda (name)
                   #t))
         (errorf 'test-env "empty environment"))
           ;;; second test:
     (or (lookup 'x
                 (extend 'x 42 mt)
                 (lambda (denotable)
                   (= denotable 42))
                 (lambda (name)
                   #f))
         (errorf 'test-env "lookup 42"))
           ;;; third test:
     (or (lookup 'x
                 (extend 'x 43 (extend 'x 42 mt))
                 (lambda (denotable)
                   (= denotable 43))
                 (lambda (name)
                   #f))
         (errorf 'test-env "lookup 43"))
           ;;; fourth test:
     (or (lookup 'y
                 (extend 'x 43 (extend 'x 42 mt))
                 (lambda (denotable)
                   #f)
                 (lambda (name)
                   #t))
         (errorf 'test-env "undeclared name"))
           ;;; add more tests here
     )))

;;;;;;;;;;

(define alist-mt
  '())

(define alist-extend
  (lambda (name denotable environment)
    (cons (cons name denotable)
          environment)))

(define alist-lookup
  (lambda (name environment found not-found)
    (letrec ([visit (lambda (e)
                      (if (null? e)
                          (not-found name)
                          (let ([binding (car e)])
                            (if (equal? name (car binding))
                                (found (cdr binding))
                                (visit (cdr e))))))])
      (visit environment))))

;;; > (test-env alist-mt alist-extend alist-lookup)
;;; #t
;;; >

;;;;;;;;;;

(define function-mt
  (lambda (name found not-found)
    (not-found name)))

(define function-lookup
  (lambda (name environment found not-found)
    (environment name found not-found)))

(define function-extend
  (lambda (new-name new-denotable given-environment)
    (lambda (name found not-found)
      (if (equal? name new-name)
          (found new-denotable)
          (function-lookup name given-environment found not-found)))))

;;; > (test-env function-mt function-extend function-lookup)
;;; #t
;;; >

;;;;;;;;;;

;;; end of week-3-environments.scm

"week-3-environments.scm"
