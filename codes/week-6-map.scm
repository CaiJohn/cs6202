;;; week-6-map.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 15 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-6-map.html

;;;;;;;;;;

(define test-map1
  (lambda (candidate)
    (and (equal? (candidate (lambda (n)
                              (* n 10))
                            '(1 2 3 4 5))
                 '(10 20 30 40 50))
         (equal? (candidate list
                            '(1 2 3 4 5))
                 '((1) (2) (3) (4) (5)))
         (equal? (candidate symbol?
                            '(a 1 #f "foo" () (hello . world)))
                 '(#t #f #f #f #f #f))
         (equal? (candidate (lambda (v)
                              (list v v))
                            '())
                 '())
         (equal? (candidate (lambda (v)
                              (list v v))
                            '(1 2 3 4 5))
                 '((1 1) (2 2) (3 3) (4 4) (5 5)))
         ;;;
         )))

(define map1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          '()
                          (cons (p (car ws))
                                (visit (cdr ws)))))])
      (visit vs))))

;;; > (test-map1 map1)
;;; #t
;;; > (test-map1 map)
;;; #t
;;; > 

;;;;;;;;;;

(define test-palindrome?
  (lambda (candidate)
    (and (equal? (candidate '())
                 #t)
         (equal? (candidate '(1))
                 #t)
         (equal? (candidate '(1 1))
                 #t)
         (equal? (candidate '(1 2 1))
                 #t)
         (equal? (candidate '(1 2 2 1))
                 #t)
         (equal? (candidate '(1 2 3 2 1))
                 #t)
         (equal? (candidate '(1 2 3 3 2 1))
                 #t)
         (let* ([ns (map (lambda (zero)
                           (random 100))
                         (make-list 100 0))]
                [ms (reverse ns)])
           (and (equal? (candidate (append ms ns))
                        #t)
                (equal? (candidate (append ms (list (random 1000)) ns))
                        #t)))
           ;;;
         )))

(define palindrome?_v0
  (lambda (vs)
    (equal? vs (reverse vs))))

;;; > (test-palindrome? palindrome?_v0)
;;; #t
;;; > 

;;;;;;;;;;

(define test-map1-append
  (lambda (candidate)
    (and (equal? (candidate (lambda (v)
                              (list v v))
                            '())
                 '())
         (equal? (candidate (lambda (v)
                              (list v v))
                            '(1 2 3 4 5))
                 '(1 1 2 2 3 3 4 4 5 5))
         (equal? (candidate list
                            '(1 2 3 4 5))
                 '(1 2 3 4 5))
         ;;;
         )))

(define map1-append
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          '()
                          (append (p (car ws))
                                  (visit (cdr ws)))))])
      (visit vs))))

;;; > (test-map1-append map1-append)
;;; #t
;;; > 

;;;;;;;;;;

(define test-andmap1
  (lambda (candidate)
    (and (equal? (candidate number? '())
                 #t)  ;;; <------******------ why not #f?
         (equal? (candidate number? '(1 2 3))
                 #t)
         (equal? (candidate number? '(1 "2" 3))
                 #f)
           ;;;
         )))

(define andmap1
  (lambda (p vs)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          #t
                          (and (p (car ws))
                               (visit (cdr ws)))))])
      (visit vs))))

;;; > (test-andmap1 andmap)
;;; #t
;;; > 

;;;;;;;;;;

(define test-ormap1
      (lambda (candidate)
        (and (equal? (candidate number? '())
                     #f)  ;;; <------******------ why not #t?
             (equal? (candidate number? '(1 2 3))
                     #t)
             (equal? (candidate number? '("1" "2" 3))
                     #t)
             (equal? (candidate number? '("1" "2" "3"))
                     #f)
             ;;;
             )))

;;;;;;;;;;

(define test-map2
  (lambda (candidate)
    (and (equal? (candidate +
                            '()
                            '())
                 '())
         (equal? (candidate +
                            '(1 2 3 4)
                            '(10 20 30 40))
                 '(11 22 33 44))
         (equal? (candidate cons
                            '(1 2 3 4)
                            '(10 20 30 40))
                 '((1 . 10) (2 . 20) (3 . 30) (4 . 40)))
         (equal? (candidate cons
                            '(x y z)
                            '(10 20 30))
                 '((x . 10) (y . 20) (z . 30)))
         (equal? (candidate list
                            '(x y z)
                            '(10 20 30))
                 '((x 10) (y 20) (z 30)))
             ;;;
         )))

;;; > (test-map2 map)
;;; #t
;;; > 

;;;;;;;;;;

(define test-cartesian-product-2
  (lambda (candidate)
    (and (equal? (candidate '() '())
                 '())
         (equal? (candidate '(a) '())
                 '())
         (equal? (candidate '() '(x))
                 '())
         (equal? (candidate '(a) '(x))
                 '((a x)))
         (equal? (candidate '(a b) '(x))
                 '((a x) (b x)))
         (equal? (candidate '(a) '(x y))
                 '((a x) (a y)))
         (equal? (candidate '(a b) '(x y))
                 '((a x) (a y) (b x) (b y)))
         (equal? (candidate '(a b) '(x y z))
                 '((a x) (a y) (a z) (b x) (b y) (b z)))
         (equal? (candidate '(a b c d) '(x y))
                 '((a x) (a y) (b x) (b y) (c x) (c y) (d x) (d y)))
         ;;;
         )))

(define cartesian-product-2
  (lambda (xs ys)
    (map1-append (lambda (x)
                   (map1-append (lambda (y)
                                  (list (list x y)))
                                ys))
                 xs)))

;;; > (test-cartesian-product-2 cartesian-product-2)
;;; #t
;;; > 

(define test-cartesian-product-3
  (lambda (candidate)
    (and (equal? (candidate '() '() '())
                 '())
         (equal? (candidate '(x y) '(a b c) '(1 2 3 4 5 6 7))
                 '((x a 1) (x a 2) (x a 3) (x a 4) (x a 5) (x a 6) (x a 7)
                   (x b 1) (x b 2) (x b 3) (x b 4) (x b 5) (x b 6) (x b 7)
                   (x c 1) (x c 2) (x c 3) (x c 4) (x c 5) (x c 6) (x c 7)
                   (y a 1) (y a 2) (y a 3) (y a 4) (y a 5) (y a 6) (y a 7)
                   (y b 1) (y b 2) (y b 3) (y b 4) (y b 5) (y b 6) (y b 7)
                   (y c 1) (y c 2) (y c 3) (y c 4) (y c 5) (y c 6) (y c 7)))
         ;;;
         )))

(define cartesian-product-3
  (lambda (xs ys zs)
    (map1-append (lambda (x)
                   (map1-append (lambda (y)
                                  (map1-append (lambda (z)
                                                 (list (list x y z)))
                                               zs))
                                ys))
                 xs)))

;;; > (test-cartesian-product-3 cartesian-product-3)
;;; #t
;;; > 

;;;;;;;;;;

(define all-possible-environments
  (lambda (xs)
    (if (null? xs)
        (list '())
        (let ([es (all-possible-environments (cdr xs))])
          (append (map (lambda (e)
                         (cons (cons (car xs) #t) e))
                       es)
                  (map (lambda (e)
                         (cons (cons (car xs) #f) e))
                       es))))))

(define test-nnf*
  (lambda (expression names)
    (let ([results (map (lambda (environment)
                          (test-nnf expression environment))
                        (all-possible-environments names))])
      (andmap1 (lambda (result)
                 (or (equal? (car results) result)
                     (errorf 'test-nnf*
                             "differing results: ~s and ~s"
                             (car results)
                             result)))
               (cdr results)))))

;;;;;;;;;;

(define test-powerset
  (lambda (candidate)
    (and (equal? (candidate '())
                 '(()))
         (equal? (candidate '(z))
                 '((z) ()))
         (equal? (candidate '(y z))
                 '((y z) (y) (z) ()))
         (equal? (candidate '(x y z))
                 '((x y z) (x y) (x z) (x) (y z) (y) (z) ()))
           ;;;
         )))

(define powerset
  (lambda (xs)
    (letrec ([visit (lambda (ys)
                      (if (null? ys)
                          (list '())
                          (let ([zss (visit (cdr ys))])
                            (append (map1 (lambda (zs)
                                            (cons (car ys) zs))
                                          zss)
                                    zss))))])
      (visit xs))))

;;; > (test-powerset powerset)
;;; #t
;;; > 

;;;;;;;;;;

(define test-append_map1
  (lambda (candidate)
    (and (equal? (candidate 1- '(1 2 3) '(10 20 30))
                 (append (map1 1- '(1 2 3)) '(10 20 30)))
           ;;;
         )))

(define append_map1
  (lambda (p vs rest)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          rest
                          (cons (p (car ws))
                                (visit (cdr ws)))))])
      (visit vs))))

;;; > (test-append_map1 append_map1)
;;; #t
;;; > 

;;;;;;;;;;

(define powerset_revisited
  (lambda (xs)
    (letrec ([visit (lambda (ys)
                      (if (null? ys)
                          (list '())
                          (let ([zss (visit (cdr ys))])
                            (append_map1 (lambda (zs)
                                           (cons (car ys) zs))
                                         zss
                                         zss))))])
      (visit xs))))

;;; > (test-powerset powerset_revisited)
;;; #t
;;; > 

(define measure_powerset
  (lambda (vs)
    (let* ([ps (time (powerset vs))]
           [ps_r (time (powerset_revisited vs))])
      (equal? ps ps_r))))

;;; > (measure_powerset '(a b c d e f g h i j k l m n))
;;; (time (powerset vs))
;;; no collections
;;; 6 ms elapsed cpu time
;;; 6 ms elapsed real time
;;; 627696 bytes allocated
;;; (time (powerset_revisited vs))
;;; no collections
;;; 4 ms elapsed cpu time
;;; 4 ms elapsed real time
;;; 264192 bytes allocated
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-6-map.scm

"week-6-map.scm"
