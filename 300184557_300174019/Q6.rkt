#lang racket

(define (longest-subsequence filename)

  (letrec ((readFile (lambda (filename)
                       (let ((p (open-input-file filename)))
                         (let f ((x (read p)))
                           (if (eof-object? x)
                               (begin
                                 (close-input-port p)
                                 '())
                               (cons x (f (read p))))))
                       )
            )

           (proc-out-file (lambda (filename proc)
                            (let ((p (open-output-file filename)))
                              (let ((v (proc p)))
                                (close-output-port p)
                              )
                            )
                           )
           )

           (output-to-file (lambda (L)
                             (proc-out-file "out.txt"
                                    (lambda (p)
                                            (let ((list-to-print L))
                                              (let f ((lp list-to-print))
                                                (if (not (null? lp))
                                                    (begin
                                                      (write (car lp) p)
                                                      (newline p)
                                                      (f (cdr lp))
                                                    )
                                                    '()
                                                 )  
                                                
                                              )
                                            )
                                    )
                             )
                             
                           )
           )

           (find-longest (lambda (filename)
                           (let* ((input-file (readFile filename))
                                  (input-len (car input-file))
                                  (input-list (cdr input-file))
                                  (max-sequence (list (car input-list)))
                                  (current-seq max-sequence)
                                 )
                             (do ((i 1 (+ i 1)))
                               ((>= i input-len)
                                (if (> (length current-seq) (length max-sequence))
                                    (output-to-file (cons (length current-seq) current-seq))
                                    (output-to-file (cons (length max-sequence) max-sequence))
                                )

                               )
                               (let ((num (list-ref input-list i))
                                     (cur-seq-last (list-ref current-seq (- (length current-seq) 1)))
                                    )
                                 (if (> num cur-seq-last)
                                     (set! current-seq (append current-seq (list num)))
                                     (if (> (length current-seq) (length max-sequence))
                                         (begin
                                           (set! max-sequence current-seq)
                                           (set! current-seq '())
                                           (set! current-seq (append current-seq (list num)))
                                         )
                                         (begin
                                           (set! current-seq '())
                                           (set! current-seq (append current-seq (list num)))
                                         )
                                     )
                                 )
                               )
                               
                             )

                             
                           )
                         )
           )
           
           )
    (find-longest filename)
    )
  
)

(longest-subsequence "in.txt")