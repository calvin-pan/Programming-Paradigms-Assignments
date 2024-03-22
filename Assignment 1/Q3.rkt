#lang racket

; This function takes three real numbers:
; The first number is in the sequence.
; The second number is the ratio.
; The third number is the number of numbers in the sequence.
; From this, the function will return a list of numbers that is the geometric sequence that is created from these numbers.

(define (geometric-sequence-loop first-number ratio n)
  (let ((result '()))
    (do ((i 0 (+ i 1)))
        ((= i n) (reverse result))
      (set! result (cons first-number result))
      (set! first-number (* first-number ratio)))))

(display "(geometric-sequence-loop 7 3 5) ==> '")
(display (geometric-sequence-loop 7 3 5)) ; Output: (7 21 63 189 567)
(newline)
(display "(geometric-sequence-loop 9 -1 7) ==> '")
(display (geometric-sequence-loop 9 -1 7)) ; Output: (9 -9 9 -9 9 -9 9)