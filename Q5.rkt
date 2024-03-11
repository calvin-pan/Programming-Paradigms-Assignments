#lang racket

; Creates a graph with only one node.
(define (make-graph)
  '((1 . ()))
)

; Creates a graph with n nodes
(define (make-graph-n n)
  (let recurse ((i n) (L '()))
    (if (<= i 0)
        L
        (recurse (- i 1) (cons (cons i '()) L))
    )
  )
)

; Add a vertex (node) to the graph



; Add the edge u, v to the given graph

; finds a path and returns it as a list of vertices from u to v, if there is a path from u to v. It returns an empty list if there is no path from u to v.

; returns the list of pairs of vertices in the graph.



















(make-graph)
(make-graph-n 5)