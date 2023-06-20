#lang racket

(require graph
        "core.rkt"
        "../dfa/image-builder.rkt"
        "../fa.rkt"
        "image-builder.rkt"
        "brzozowski-minimization.rkt"
        "dfa-operations.rkt")


(define (automaton-difference dfa1 dfa2)
    (mk-intersection-dfa dfa1 (complement dfa2)))

(define T
  (dfa A (B C) (A : 0 -> C)
       (A : 1 -> B)
       (B : 0 -> D)
       (B : 1 -> A)
       (C : 1 -> D)
       (C : 0 -> A)
       (D : 0 -> B)
       (D : 1 -> C)))

;;; (print T) 

(define (transform-dfa-graph dfa)
    (map (lambda (t)
        (list (cdr (car t)) (car (car t)) (cdr t))) (dfa-delta dfa)))
        

(transform-dfa-graph T) 


(define g (weighted-graph/directed (transform-dfa-graph T)))
;;; (bfs g (dfa-start T))

(define start-vertex (dfa-start T))
(define end-vertex (dfa-final T))
(dijkstra g (dfa-start T))
