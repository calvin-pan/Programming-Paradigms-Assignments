#lang racket

(define (hidden-words str lst)
  (define (hidden? word)
    (let loop ((str-characters (string->list str))
               (word-characters (string->list word)))
      (cond ((null? word-characters) #t)
            ((null? str-characters) #f)
            ((char=? (car str-characters) (car word-characters))
             (loop (cdr str-characters) (cdr word-characters)))
            (else (loop (cdr str-characters) word-characters)))))
  
  (filter hidden? lst))

(display "(hidden-words “subdermatoglyphic”, '(“set”, “graphic”, “drama”, “toy”, “brag”)) ==> '")
(display (hidden-words "subdermatoglyphic" '("set" "graphic" "drama" "toy" "brag")))