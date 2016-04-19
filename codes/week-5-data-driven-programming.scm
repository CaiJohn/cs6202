;;; week-5-data-driven-programming.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 07 Sep 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-5-data-driven-programming.html

;;;;;;;;;;

;;; ns ::= () | (n . ns)
;;; n  ::= ...any Scheme number...

(define test-proper-list-of-numbers?
  (lambda (candidate)
    (and ;;; proper lists of numbers:
         (equal? (candidate '())
                 #t)
         (equal? (candidate '(1))
                 #t)
         (equal? (candidate '(1 2))
                 #t)
         (equal? (candidate '(1 1 1))
                 #t)
         ;;; any other Scheme value:
         (equal? (candidate 'DAIMI)
                 #f)
         (equal? (candidate 42)
                 #f)
         (equal? (candidate '(1 "2" 3 4))
                 #f)
         (equal? (candidate '(1 2 3 . 4))
                 #f)
         ;;;
     )))

(define proper-list-of-numbers?
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(null? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (visit (cdr v)))]
                        [else
                         #f]))])
      (visit v_init))))

;;; > (test-proper-list-of-numbers? proper-list-of-numbers?)
;;; #t
;;; > 

;;;;;;;;;;

(define test-proper-list-of-distinct-numbers?
  (lambda (candidate)
    (and ;;; proper lists of distinct numbers:
     (equal? (candidate '())
             #t)
     (equal? (candidate '(1))
             #t)
     (equal? (candidate '(1 2))
             #t)
           ;;; any other Scheme value:
     (equal? (candidate '(1 1 1))
             #f)
     (equal? (candidate 'DAIMI)
             #f)
     (equal? (candidate 42)
             #f)
     (equal? (candidate '(1 "2" 3 4))
             #f)
     (equal? (candidate '(1 2 3 . 4))
             #f)
     (equal? (candidate '(0 1 2 3 4 5 6 7 8 9 0))
             #f)
           ;;;
     )))

;;;;;;;;;;

(define proper-list-of-distinct-numbers?_v0
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(null? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (if (member (car v) (cdr v))
                                  #f
                                  (visit (cdr v))))]
                        [else
                         #f]))])
      (visit v_init))))

;;; > (test-proper-list-of-distinct-numbers? proper-list-of-distinct-numbers?_v0)
;;; 
;;; Exception in member: improper list (2 3 . 4)
;;; Type (debug) to enter the debugger.
;;; > 

;;;;;;;;;;

(define proper-list-of-distinct-numbers?_v1
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(null? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (if (member (car v) a)
                                  #f
                                  (visit (cdr v) (cons (car v) a))))]
                        [else
                         #f]))])
      (visit v_init '()))))

;;; > (test-proper-list-of-distinct-numbers? proper-list-of-distinct-numbers?_v1)
;;; #t
;;; > 

;;;;;;;;;;

(define test-member?
  (lambda (candidate)
    (and ;;; an element occurs as a car or as the last cdr:
         (equal? (candidate 1 '(0 1 2 3))
                 #t)
         (equal? (candidate 1 '(0 1 2 3 . 4))
                 #t)
         (equal? (candidate 4 '(0 1 2 3 . 4))
                 #t)
         (equal? (candidate 1 '(0 "42" 1))
                 #t)
         (equal? (candidate 1 '(0 "42" . 1))
                 #t)
         (equal? (candidate 1 1)
                 #t)
         ;;; an element does not occur as a car or as the last cdr:
         (equal? (candidate 1 '())
                 #f)
         (equal? (candidate 4 '(0 1 2 3))
                 #f)
         (equal? (candidate 4 '(0 1 2 . 3))
                 #f)
         (equal? (candidate 1 2)
                 #f)
         ;;; corny corner cases:
         (equal? (candidate '() '(1 () 3))
                 #t)
         (equal? (candidate '() '(1 3))
                 #f)
         ;;;
         )))

(define member?
  (lambda (x xs_init)
    (letrec ([visit (lambda (xs)
                      (cond
                        [(null? xs)
                         #f] 
                       [(pair? xs)
                         (or (equal? x (car xs))
                             (visit (cdr xs)))]
                        [else
                         (equal? x xs)]))])
      (visit xs_init))))

;;; > (test-member? member?)
;;; #t
;;; > 

;;;;;;;;;;

(define proper-list-of-distinct-numbers?_v2
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(null? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (if (member? (car v) (cdr v))
                                  #f
                                  (visit (cdr v))))]
                        [else
                         #f]))])
      (visit v_init))))

;;; > (test-proper-list-of-distinct-numbers? proper-list-of-distinct-numbers?_v2)
;;; #t
;;; > 

(define proper-list-of-distinct-numbers?_v3
  (lambda (v_init)
    (letrec ([visit (lambda (v a)
                      (cond
                        [(null? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (if (member? (car v) a)
                                  #f
                                  (visit (cdr v) (cons (car v) a))))]
                        [else
                         #f]))])
      (visit v_init '()))))

(define from-to
  (lambda (min max)
    (letrec ([visit (lambda (n)
                      (if (= n max)
                          (list max)
                          (cons n (visit (1+ n)))))])
      (if (> min max)
          '()
          (visit min)))))

;;; > (from-to 10 12)
;;; (10 11 12)
;;; > (from-to 10 10)
;;; (10)
;;; > (from-to 10 8)
;;; ()
;;; > 

;;; > (define 1-1000 (from-to 1 1000))
;;; > (equal? (time (proper-list-of-distinct-numbers?_v3 (from-to 1 1000))) (time (proper-list-of-distinct-numbers?_v2 (from-to 1 1000))))
;;; (time (proper-list-of-distinct-numbers?_v2 \x31;-1000))
;;;     no collections
;;;     126 ms elapsed cpu time
;;;     131 ms elapsed real time
;;;     88056 bytes allocated
;;; (time (proper-list-of-distinct-numbers?_v3 \x31;-1000))
;;;     no collections
;;;     133 ms elapsed cpu time
;;;     133 ms elapsed real time
;;;     96056 bytes allocated
;;; #t
;;; > (define 1-10000 (from-to 1 10000))
;;; > (equal? (time (proper-list-of-distinct-numbers?_v3 1-10000)) (time (proper-list-of-distinct-numbers?_v2 1-10000)))
;;; (time (proper-list-of-distinct-numbers?_v2 \x31;-10000))
;;;     no collections
;;;     11967 ms elapsed cpu time
;;;     11977 ms elapsed real time
;;;     880056 bytes allocated
;;; (time (proper-list-of-distinct-numbers?_v3 \x31;-10000))
;;;     no collections
;;;     12108 ms elapsed cpu time
;;;     12120 ms elapsed real time
;;;     960056 bytes allocated
;;; #t
;;; > 

;;;;;;;;;;

(define try-me-transparently
  (lambda (candidate input expected-output name)
    (or (equal? (candidate input)
                expected-output)
        (begin
          (printf "~s: error for ~s~n" name input)
          #f))))

(define and-all
    (lambda bs_init
      (letrec ([visit (lambda (bs)
                        (or (null? bs)
                            (and (car bs)
                                 (visit (cdr bs)))))])
        (visit bs_init))))

;;; ns ::= () | n | (n . ns)

(define test-list-of-numbers?
  (lambda (candidate)
    (and-all
     ;;; lists of numbers:
     (try-me-transparently candidate '() #t 'list-of-numbers?)
     (try-me-transparently candidate '(1) #t 'list-of-numbers?)
     (try-me-transparently candidate '(1 2) #t 'list-of-numbers?)
     (try-me-transparently candidate '(1 1 1) #t 'list-of-numbers?)
     (try-me-transparently candidate '42 #t 'list-of-numbers?)
     (try-me-transparently candidate '(1 2 3 . 4) #t 'list-of-numbers?)
     ;;; any other Scheme value:
     (try-me-transparently candidate '(1 "2" 3 4) #f 'list-of-numbers?)
     (try-me-transparently candidate '(1 2 . "3") #f 'list-of-numbers?)
     (try-me-transparently candidate 'DAIM #f 'list-of-numbers?)
     ;;;
     )))

(define list-of-numbers?
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(null? v)
                         #t]
                        [(number? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (visit (cdr v)))]
                        [else
                         #f]))])
      (visit v_init))))

;;; > (test-list-of-numbers? list-of-numbers?)
;;; #t
;;; > 

;;;;;;;;;;

(define test-list-of-distinct-numbers?
  (lambda (candidate)
    (and-all
     ;;; lists of numbers:
     (try-me-transparently candidate '() #t 'list-of-numbers?)
     (try-me-transparently candidate '(1) #t 'list-of-numbers?)
     (try-me-transparently candidate '(1 2) #t 'list-of-numbers?)
     (try-me-transparently candidate '42 #t 'list-of-numbers?)
     (try-me-transparently candidate '(1 2 3 . 4) #t 'list-of-numbers?)
     ;;; any other Scheme value:
     (try-me-transparently candidate '(1 1 1) #f 'list-of-numbers?)
     (try-me-transparently candidate '(1 1 1 . 1) #f 'list-of-numbers?)
     (try-me-transparently candidate '(1 "2" 3 4) #f 'list-of-numbers?)
     (try-me-transparently candidate '(1 2 . "3") #f 'list-of-numbers?)
     (try-me-transparently candidate 'DAIM #f 'list-of-numbers?)
     ;;;
     )))

(define list-of-distinct-numbers?
  (lambda (v_init)
    (letrec ([visit (lambda (v)
                      (cond
                        [(null? v)
                         #t]
                        [(number? v)
                         #t]
                        [(pair? v)
                         (and (number? (car v))
                              (if (member? (car v) (cdr v))
                                  #f
                                  (visit (cdr v))))]
                        [else
                         #f]))])
      (visit v_init))))

;;; > (test-list-of-distinct-numbers? list-of-distinct-numbers?)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-5-data-driven-programming.scm

"week-5-data-driven-programming.scm"
