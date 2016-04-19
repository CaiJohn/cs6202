;;; week-3-equality-procedures.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 25 Aug 2015

;;; Accompanying material for the lecture notes at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-3.html

;;;;;;;;;;

;;; from week-2.scm:

(define test-equal?
  (lambda (candidate)
    (and
     ;;; numbers:
     (equal? (candidate 2
                        2)
             #t)
     (equal? (candidate 2
                        3)
             #f)
     (equal? (candidate 2
                        #t)
             #f)
     ;;; Booleans:
     (equal? (candidate #t
                        #t)
             #t)
     (equal? (candidate #f
                        #f)
             #t)
     (equal? (candidate #t
                        #f)
             #f)
     (equal? (candidate #t
                        33)
             #f)
     ;;; characters:
     (equal? (candidate #\c
                        #\c)
             #t)
     (equal? (candidate #\c
                        #\d)
             #f)
     (equal? (candidate #\c
                        33)
             #f)
     ;;; strings
     (equal? (candidate "hello"
                        "hello")
             #t)
     (equal? (candidate "hello"
                        "hola")
             #f)
     (equal? (candidate "hello"
                        33)
             #f)
     (equal? (candidate "42"
                        42)
             #f)
     ;;; procedures
     (equal? (candidate (lambda (x) x)
                        (lambda (y) y))
             #f)
     ;;; pairs
     (equal? (candidate (cons 1 2)
                        (cons 1 2))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 4)))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 4 3)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 2 1) (cons 3 4)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 #f)))
             #f)
     ;;; add more tests here
     )))

;;; > (test-equal? equal?)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_revisited
  (lambda (v1 v2)
    (if (number? v1)
        (if (number? v2)
            (= v1 v2)
            #f)
        (if (boolean? v1)
            (if (boolean? v2)
                (boolean=? v1 v2)
                #f)
            (if (char? v1)
                (if (char? v2)
                    (char=? v1 v2)
                    #f)
                (if (string? v1)
                    (if (string? v2)
                        (string=? v1 v2)
                        #f)
                    (if (pair? v1)
                        (if (pair? v2)
                            (if (equal?_revisited (car v1) (car v2))
                                (equal?_revisited (cdr v1) (cdr v2))
                                #f)
                            #f)
                        #f)))))))

;;; > (test-equal? equal?_revisited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re2visited
  (lambda (v1 v2)
    (cond
      [(number? v1)
       (if (number? v2)
           (= v1 v2)
           #f)]
      [(boolean? v1)
       (if (boolean? v2)
           (boolean=? v1 v2)
           #f)]
      [(char? v1)
       (if (char? v2)
           (char=? v1 v2)
           #f)]
      [(string? v1)
       (if (string? v2)
           (string=? v1 v2)
           #f)]
      [(pair? v1)
       (if (pair? v2)
           (if (equal?_re2visited (car v1) (car v2))
               (equal?_re2visited (cdr v1) (cdr v2))
               #f)
           #f)]
      [else
       #f])))

;;; > (test-equal? equal?_re2visited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re3visited
  (lambda (v1 v2)
    (cond
      [(number? v1)
       (and (number? v2)
            (= v1 v2))]
      [(boolean? v1)
       (and (boolean? v2)
            (boolean=? v1 v2))]
      [(char? v1)
       (and (char? v2)
            (char=? v1 v2))]
      [(string? v1)
       (and (string? v2)
            (string=? v1 v2))]
      [(pair? v1)
       (and (pair? v2)
            (equal?_re3visited (car v1) (car v2))
            (equal?_re3visited (cdr v1) (cdr v2)))]
      [else
       #f])))

;;; > (test-equal? equal?_re3visited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re3visited_alt
  (lambda (v1 v2)
    (cond
      [(and (number? v1) (number? v2))
       (= v1 v2)]
      [(and (boolean? v1) (boolean? v2))
       (boolean=? v1 v2)]
      [(and (char? v1) (char? v2))
       (char=? v1 v2)]
      [(and (string? v1) (string? v2))
       (string=? v1 v2)]
      [(and (pair? v1) (pair? v2))
       (and (equal?_re3visited_alt (car v1) (car v2))
            (equal?_re3visited_alt (cdr v1) (cdr v2)))]
      [else
       #f])))

;;; > (test-equal? equal?_re3visited_alt)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re4visited
  (lambda (v1 v2)
    (or (and (number? v1)
             (number? v2)
             (= v1 v2))
        (and (boolean? v1)
             (boolean? v2)
             (boolean=? v1 v2))
        (and (char? v1)
             (char? v2)
             (char=? v1 v2))
        (and (string? v1)
             (string? v2)
             (string=? v1 v2))
        (and (pair? v1)
             (pair? v2)
             (equal?_re4visited (car v1) (car v2))
             (equal?_re4visited (cdr v1) (cdr v2))))))

;;;> (test-equal? equal?_re4visited)
;;;#t
;;;> 

;;;;;;;;;;

;;;; Exercise 9

(define test-equal?
  (lambda (candidate)
    (and 
     ;;; numbers:
     (equal? (candidate 2
                        2)
             #t)
     (equal? (candidate 2
                        3)
             #f)
     (equal? (candidate 2
                        #t)
             #f)
     ;;; Booleans:
     (equal? (candidate #t
                        #t)
             #t)
     (equal? (candidate #f
                        #f)
             #t)
     (equal? (candidate #t
                        #f)
             #f)
     (equal? (candidate #t
                        33)
             #f)
     ;;; characters:
     (equal? (candidate #\c
                        #\c)
             #t)
     (equal? (candidate #\c
                        #\d)
             #f)
     (equal? (candidate #\c
                        33)
             #f)
     ;;; strings
     (equal? (candidate "hello"
                        "hello")
             #t)
     (equal? (candidate "hello"
                        "hola")
             #f)
     (equal? (candidate "hello"
                        33)
             #f)
     (equal? (candidate "42"
                        42)
             #f)
     ;;; symbols
     (equal? (candidate 'foo
                        'foo)
             #t)
     (equal? (candidate 'foo
                        'bar)
             #f)
     (equal? (candidate 'foo
                        "foo")
             #f)
     ;;; procedures
     (equal? (candidate (lambda (x) x)
                        (lambda (y) y))
             #f)
     ;;; the empty list
     (equal? (candidate '()
                        (list))
             #t)
     (equal? (candidate '()
                        33)
             #f)
     (equal? (candidate '()
                        (cons 'foo 'bar))
             #f)
     ;;; pairs
     (equal? (candidate (cons 1 2)
                        (cons 1 2))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 4)))
             #t)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 4 3)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 2 1) (cons 3 4)))
             #f)
     (equal? (candidate (cons (cons 1 2) (cons 3 4))
                        (cons (cons 1 2) (cons 3 #f)))
             #f)
     ;;; add more tests here
     )))

;;;;;;;;;;

(define equal?_re5visited
  (lambda (v1 v2)
    (letrec ([visit
              (lambda (v1 v2)
                (cond
                  [(number? v1)
                   (and (number? v2)
                        (= v1 v2))]
                  [(boolean? v1)
                   (and (boolean? v2)
                        (boolean=? v1 v2))]
                  [(char? v1)
                   (and (char? v2)
                        (char=? v1 v2))]
                  [(string? v1)
                   (and (string? v2)
                        (string=? v1 v2))]
                  [(symbol? v1)
                   (and (symbol? v2)
                        (symbol=? v1 v2))]
                  [(null? v1)
                   (null? v2)]
                  [(pair? v1)
                   (and (pair? v2)
                        (visit (car v1) (car v2))
                        (visit (cdr v1) (cdr v2)))]
                  [else
                   #f]))])
      (visit v1 v2))))

;;; > (test-equal? equal?_re5visited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re6visited
  (lambda (v1 v2)
    (letrec ([visit
              (lambda (v1 v2)
                (cond
                  [(eq? v1 v2)
                   #t]
                  [(string? v1)
                   (and (string? v2)
                        (string=? v1 v2))]
                  [(pair? v1)
                   (and (pair? v2)
                        (visit (car v1) (car v2))
                        (visit (cdr v1) (cdr v2)))]
                  [else
                   #f]))])
      (visit v1 v2))))

;;; > (test-equal? equal?_re6visited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re7visited
  (lambda (v1 v2)
    (letrec ([visit
              (lambda (v1 v2)
                (cond
                  [(eq? v1 v2)
                   #t]
                  [(string? v1)
                   (and (string? v2)
                        (string=? v1 v2))]
                  [(pair? v1)
                   (and (pair? v2)
                        (let ([d1 (cdr v1)]
                              [d2 (cdr v2)])
                          (if (eq? d1 d2)
                              (visit (car v1) (car v2))
                              (and (visit (car v1) (car v2))
                                   (visit (cdr v1) (cdr v2))))))]
                  [else
                   #f]))])
      (visit v1 v2))))

;;; > (test-equal? equal?_re7visited)
;;; #t
;;; > 

;;;;;;;;;;

(define equal?_re8visited
  (lambda (v1 v2)
    (letrec ([visit
              (lambda (v1 v2 continue)
                (cond
                  [(eq? v1 v2)
                   (continue)]
                  [(string? v1)
                   (and (string? v2)
                        (string=? v1 v2)
                        (continue))]
                  [(pair? v1)
                   (and (pair? v2)
                        (visit (car v1)
                               (car v2)
                               (lambda ()
                                 (visit (cdr v1)
                                        (cdr v2)
                                        continue))))]
                  [else
                   #f]))])
      (visit v1 v2 (lambda ()
                     #t)))))

;;; > (test-equal? equal?_re8visited)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-3-equality-procedures.scm

"week-3-equality-procedures.scm"
