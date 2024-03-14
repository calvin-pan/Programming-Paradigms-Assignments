#lang racket

; We can represent an undirected graph in the form ((node/vertex (connected nodes/vertices))):
; E.g. '((1 (2 3)) (2 (1)) (3 (1)) (4 ()))

; Creates a graph with only one node.
(define make-graph '((1 ())))

; Creates a graph with n nodes
(define (make-graph-n n)
  (let recurse ((i n) (L '()))
    (if (< i 0)
        (reverse L)
        (recurse (- i 1) (append L (list (list i (list)))))
    )
  )
)

; Add a vertex (node) to the graph
(define (add-vertex g)
  ;count node numbers
  (let ((next-node (+ (car (car (reverse g))) 1)))
    (append g (list (list next-node (list))))
  )
 
)
; Add edge uv to the given graph g
; since add-vertex, make-graph-n, and make-graph are all built on the assumption that the graphs are sequential (1, 2, 3, 4, 5, etc.) then,
; we can append v to the sublist in node u (node u = g at index (u - 1)) and vice versa.
(define (add-edge g u v)
  (let ((new-graph g))
    (begin
         (set! new-graph (list-set new-graph (- u 1) (list u (append (list-ref (list-ref new-graph (- u 1)) 1) (list v))))) ; append v to sublist of node u and set new sublist to node n
         (set! new-graph (list-set new-graph (- v 1) (list v (append (list-ref (list-ref new-graph (- v 1)) 1) (list u))))) ; append u to sublist of node v and set new sublist to node v
         new-graph
    )
  )

)

; finds a path and returns it as a list of vertices from u to v, if there is a path from u to v. It returns an empty list if there is no path from u to v.
; use BFS
(define (find-path g u v)
  (letrec ((initialise (lambda (g src dest)
                         (let ((queue (list (list src))))
                           (while g src dest queue)
                         )

                      )
           )
           ; Helper function that returns the adjacency list of the node. Returns an empty list if the node has no adjacent nodes.
           (get-node-list (lambda (g node) 
                            (if (equal? (assoc node g) #f)
                                '()
                                (let* ((elem (assoc node g))
                                       (node-list (car (cdr elem)))
                                       )
                                  node-list
                                )
                            )
                          )
           )

           (while (lambda (g src dest queue)
                    (if (null? queue) ; If the queue is null, then there is no path. 
                        '()
                        (let* ((queue-pop (car queue))
                               (new-queue (cdr queue))
                               (path queue-pop)
                               (node (list-ref queue-pop (- (length queue-pop) 1)))
                               )

                          (if (= node dest)
                              path

                              (let* ((node-list (get-node-list g node))
                                     (node-list-len (length node-list))
                                     )
                                (if (= node-list-len 0)
                                    (while g src dest new-queue)
                                    (do ((i 0 (+ i 1))) ; Iterate over the node's adjacency list. 
                                      ((>= i node-list-len) (while g src dest new-queue))
                                      (let* ((adj (car node-list))
                                             (new-path (append path (list adj))) ;Append each adjacent node to the current path in a new path.
                                             )
                                        (begin
                                          (set! node-list (cdr node-list)) 
                                          (set! new-queue (append new-queue (list new-path))) ; Append the new path to the queue.
                                          )
                                        )
                                      )
                                    )
                                )
                              ) 
                          )
                    )                
                  )
           )      
          )
    (initialise g u v)
  )
)


; returns the list of pairs of vertices in the graph.
(define (edges g)
  (letrec ((while (lambda (g pairs)
                    (if (null? g)
                        pairs
                        (let* ((elem (car g))
                               (node (car elem))
                               (adj-list (car (cdr elem)))
                               (adj-list-len (length adj-list))
                               (new-pairs pairs)
                              )
                          
                          (do ((i 0 (+ i 1)))
                            ((>= i adj-list-len) (while (cdr g) new-pairs))
                            (set! new-pairs (append new-pairs (list (cons node (car adj-list)))))
                            (set! adj-list (cdr adj-list))
                          )
                          
                        )
                    )
                  )
           )
          )
          
    (while g '())
  )
)

make-graph
(make-graph-n 5)
(add-vertex '((1 (2 3)) (2 (1)) (3 (1))))
(add-edge '((1 (2 3)) (2 (1)) (3 (1))) 3 2)
(find-path '((1 (2 3 4)) (2 (5 6)) (5 (9 10)) (4 (7 8)) (7 (11 12))) 1 11)
(find-path (make-graph-n 11) 1 11)
(edges '((1 (2 3 4)) (2 (5 6)) (5 (9 10)) (4 (7 8)) (7 (11 12))))
(edges (make-graph-n 5))
