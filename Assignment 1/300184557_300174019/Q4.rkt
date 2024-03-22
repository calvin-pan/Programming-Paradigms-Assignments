#lang racket

; This function implements heap sort.

(define (heap-sort vec)
  (define (heapify vec n i)
    (let* ((largest i)
           (left (+ (* 2 i) 1))
           (right (+ (* 2 i) 2)))
      (when (and (< left n) (> (vector-ref vec left) (vector-ref vec largest)))
        (set! largest left))
      (when (and (< right n) (> (vector-ref vec right) (vector-ref vec largest)))
        (set! largest right))
      (when (not (= largest i))
        (let ((temp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec largest))
          (vector-set! vec largest temp)
          (heapify vec n largest)))))

  ; This function builds the heap.

  (define (build-heap vec)
    (let ((n (vector-length vec)))
      (do ((i (quotient n 2) (- i 1)))
          ((< i 0) vec)
        (heapify vec n i))))

  ; This function sorts the heap.

  (define (sort vec)
    (let ((mutable-vec (vector-copy vec))
          (n (vector-length vec)))
      (build-heap mutable-vec)
      (do ((i (- n 1) (- i 1)))
          ((< i 0) mutable-vec)
        (let ((temp (vector-ref mutable-vec 0)))
          (vector-set! mutable-vec 0 (vector-ref mutable-vec i))
          (vector-set! mutable-vec i temp)
          (heapify mutable-vec i 0)))))

  (sort vec))

(display "(heap-sort '#(10 18 3 22 9 31 45 28)) ==> '")
(display (heap-sort #(10 18 3 22 9 31 45 28))) ; Output: #(3 9 10 18 22 28 31 45)
(newline)

; This function implements insertion sort.

(define (insertion-sort vec)
  (define (insert val vec)
    (cond ((null? vec) (list val))
          ((<= val (car vec)) (cons val vec))
          (else (cons (car vec) (insert val (cdr vec))))))
  
  (define (insertion-sort-helper vec sorted)
    (if (null? vec)
        sorted
        (insertion-sort-helper (cdr vec) (insert (car vec) sorted))))
  
  (apply vector (insertion-sort-helper (vector->list vec) '())))

;; Example usage:
(display "(insertion-sort '#(10 18 3 22 9 31 45 28)) ==> '")
(display (insertion-sort '#(10 18 3 22 9 31 45 28)))







