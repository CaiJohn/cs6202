;;; week-3-finite-state-automaton.scm
;;; CS6202 2015-2016, S1
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 25 Aug 2015

;;; Accompanying material for the lecture notes at
;;;   http://users-cs.au.dk/danvy/CS6202/Lecture-notes/week-3.html

;;;;;;;;;;

(define test-odd-number-of-ones?
  (lambda (candidate)
    (and (equal? (candidate "1")
                 "yes")
         (equal? (candidate "001000")
                 "yes")
         (equal? (candidate "00111000")
                 "yes")
         (equal? (candidate "0010101000")
                 "yes")
         (equal? (candidate "")
                 "stuck")
         (equal? (candidate "0")
                 "stuck")
         (equal? (candidate "11")
                 "stuck")
         (equal? (candidate "00101010001")
                 "stuck")
         (equal? (candidate "001010100010")
                 "stuck")
         (equal? (candidate "xy")
                 "error")
         (equal? (candidate "0xy1")
                 "error")
         (equal? (candidate "1xy0")
                 "error")
           ;;; add more tests here
         )))

;;;;;;;;;;

(define state-with-even-number-of-ones
  (lambda (s i max)
    (if (= i max)
        "stuck"
        (if (char=? (string-ref s i) #\0)
            (state-with-even-number-of-ones s (1+ i) max)
            (if (char=? (string-ref s i) #\1)
                (state-with-odd-number-of-ones s (1+ i) max)
                "error")))))

(define state-with-odd-number-of-ones
  (lambda (s i max)
    (if (= i max)
        "yes"
        (if (char=? (string-ref s i) #\0)
            (state-with-odd-number-of-ones s (1+ i) max)
            (if (char=? (string-ref s i) #\1)
                (state-with-even-number-of-ones s (1+ i) max)
                "error")))))

(define odd-number-of-ones?
  (lambda (s)
    (state-with-even-number-of-ones s 0 (string-length s))))

;;;;;;;;;;

(define state
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (if (char=? (string-ref s i) #\0)
            (state s (1+ i) max b)
            (if (char=? (string-ref s i) #\1)
                (state s (1+ i) max (not b))
                "error")))))

(define other-odd-number-of-ones?
  (lambda (s)
    (state s 0 (string-length s) #f)))

;;;;;;;;;;

(define state_revisited
  (lambda (s i max b)
    (cond
      [(= i max)
       (if b
           "yes"
           "stuck")]
      [(char=? (string-ref s i) #\0)
       (state_revisited s (1+ i) max b)]
      [(char=? (string-ref s i) #\1)
       (state_revisited s (1+ i) max (not b))]
      [else
       "error"])))

(define other-odd-number-of-ones?_revisited
  (lambda (s)
    (state_revisited s 0 (string-length s) #f)))

;;; > (test-odd-number-of-ones? other-odd-number-of-ones?_revisited)
;;; #t
;;; > 

;;;;;;;;;;

(define odd-number-of-ones?_revisited
  (lambda (s)
    (let ([max (string-length s)])
      (letrec ([state-with-even-number-of-ones
                (lambda (i)
                  (if (= i max)
                      "stuck"
                      (case (string-ref s i)
                        [(#\0)
                         (state-with-even-number-of-ones (1+ i))]
                        [(#\1)
                         (state-with-odd-number-of-ones (1+ i))]
                        [else
                         "error"])))]
               [state-with-odd-number-of-ones
                (lambda (i)
                  (if (= i max)
                      "yes"
                      (case (string-ref s i)
                        [(#\0)
                         (state-with-odd-number-of-ones (1+ i))]
                        [(#\1)
                         (state-with-even-number-of-ones (1+ i))]
                        [else
                         "error"])))])
        (state-with-even-number-of-ones 0)))))

;;; > (test-odd-number-of-ones? odd-number-of-ones?_revisited)
;;; #t
;;; > 

;;;;;;;;;;

(define other-odd-number-of-ones?_revisited
  (lambda (s)
    (let ([max (string-length s)])
      (letrec ([state
                (lambda (i b)
                  (if (= i max)
                      (if b
                          "yes"
                          "stuck")
                      (case (string-ref s i)
                        [(#\0)
                         (state (1+ i) b)]
                        [(#\1)
                         (state (1+ i) (not b))]
                        [else
                         "error"])))])
        (state 0 #f)))))

;;; > (test-odd-number-of-ones? other-odd-number-of-ones?_revisited)
;;; #t
;;; > 

;;;;;;;;;;

;;; end of week-3-finite-state-automaton.scm

"week-3-finite-state-automaton.scm"
