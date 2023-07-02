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

(define (transform-dfa-graph dfa)
    (map (lambda (t)
        (list (cdr (car t)) (car (car t)) (cdr t))) (dfa-delta dfa)))

(define T
  (dfa A (B D) (A : 0 -> C)
       (A : 1 -> B)
       (B : 0 -> D)
       (B : 1 -> A)
       (C : 1 -> D)
       (C : 0 -> A)
       (D : 0 -> B)
       (D : 1 -> C)))

(define P
  (dfa p1 (p4)
       (p1 : 0 -> p2)
       (p1 : 1 -> p1)
       (p2 : 0 -> p3)
       (p2 : 1 -> p2)
       (p3 : 1 -> p3)
       (p3 : 0 -> p4)
       (p4 : 1 -> p4)
       (p4 : 0 -> p3)))

(define N
  (dfa C (D)
       (C : 0 -> D)
       (C : 1 -> D)
       (D : 0 -> C)
       (D : 1 -> C)))


(define (find-path predecessors state word graph)
  (if (hash-ref predecessors state)
    (find-path predecessors 
      (hash-ref predecessors state) 
      (append word 
        (list 
          (edge-weight graph state 
            (hash-ref predecessors state))))
      graph)
    word))

(define (acceptance-words predecessors final-states graph)
  (define words
    (map (lambda (state)
           (find-path predecessors state empty graph))
         final-states))
  (map (lambda (letters)
         (apply string-append 
          (map (lambda (letter)
            (format "~a" letter)) letters)))
       words))

(define (find-acceptance-words dfa)
  (define graph (weighted-graph/directed 
    (transform-dfa-graph dfa)))
  (define-values (distance predecessors) 
    (bfs graph (dfa-start dfa)))
  (acceptance-words predecessors (dfa-final dfa) graph))

(define (automaton-correction answer feedback)
  (define dfa1 (minimization (automaton-difference answer feedback)))
  (define dfa2 (minimization (automaton-difference feedback answer)))
  (cond 
    [(not (empty? (dfa-final dfa1))) (find-acceptance-words dfa1)]
    [(not (empty? (dfa-final dfa2))) (find-acceptance-words dfa2)]
    [else (displayln "Your automaton accept the same words of the feedback")]))

(automaton-correction T P)

(dfa-final (automaton-difference P T))

(dfa-final (minimization (automaton-difference P T)))