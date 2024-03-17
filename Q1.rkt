#lang racket

; This function takes three real numbers that represent the coefficients of a quadratic equation and returns a list of its roots.
; It uses the quadratic formula to calculate the roots.

(define(solve-quadratic a b c)
  (let ((discriminant (- (* b b) (* 4 a c))))
    (cond ((> discriminant 0)
        (list (/(round(*(/(+ (- b)(sqrt discriminant))(* 2 a)) 1000))1000) (/(round(*(/(- (- b) (sqrt discriminant))(* 2 a)) 1000))1000)))
          ((= discriminant 0)
           (list (/(round(*(/(+ (- b)(sqrt discriminant))(* 2 a)) 1000))1000)))
          (else '()))))

(display "(solve-quadratic 4 5 -10) ==> '")
(display (solve-quadratic 4 5 -10))
(newline)
(display "(solve-quadratic 1 -2 1) ==> '")
(display (solve-quadratic 1 -2 1))
(newline)
(display "(solve-quadratic 3 2 1) ==> '")
(display (solve-quadratic 3 2 1))