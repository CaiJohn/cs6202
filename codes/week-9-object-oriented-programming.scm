;;; week-20-object-oriented-programming.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 Oct 2015

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/CS56202/Lecture-notes/week-9-object-oriented-programming.html

;;;;;;;;;;

(define make-counter
  (lambda (init)
    (let ([counter init])
      (lambda (message)
        (case message
          [(val)
           (list counter)]
          [(inc)
           (begin
             (set! counter (1+ counter))
             (list))]
          [(dec)
           (begin
             (set! counter (1- counter))
             (list))]
          [(init?)
           (list (= counter init))]
          [else
           (list "What was that?")])))))

(define make-counter_alt
  (lambda (init)
    (lambda (message)       ;;; <---***---
      (let ([counter init]) ;;; <---***---
        (case message
          [(val)
           (list counter)]
          [(inc)
           (begin
             (set! counter (1+ counter))
             (list))]
          [(dec)
           (begin
             (set! counter (1- counter))
             (list))]
          [(init?)
           (list (= counter init))]
          [else
           (list "What was that?")])))))

(define make-adjustable-counter
  (lambda (init)
    (let ([counter init])
      (lambda (message)
        (case message
          [(val)
           (list counter)]
          [(inc)
           (lambda (increment)
             (begin
               (set! counter (+ counter increment))
               (list)))]
          [(dec)
           (lambda (decrement)
             (begin
               (set! counter (- counter decrement))
               (list)))]
          [(init?)
           (list (= counter init))]
          [else
           (list "What was that?")])))))

(define colorize
  (lambda (object initial-color)
    (let ([current-color initial-color])
      (lambda (message)
        (case message
          [(color)
           (list current-color)]
          [else
           (object message)])))))

;;;;;;;;;;

;;; end of week-20-object-oriented-programming.scm
