#lang racket

(require graph
        "core.rkt"
        "../dfa/image-builder.rkt"
        "../fa.rkt"
        "image-builder.rkt"
        "brzozowski-minimization.rkt"
        "table-minimization.rkt"
        "dfa-operations.rkt")

(provide automaton-difference
         automaton-correction)

(define (automaton-difference dfa1 dfa2)
    (mk-intersection-dfa dfa1 (complement dfa2)))

;; function that transform the automaton to a data struct that can build a weighted-graph/directed
(define (transform-dfa-graph dfa)
    (map (lambda (t)
        (list (cdr (car t)) (car (car t)) (cdr t))) (dfa-delta dfa)))

;; function that walk on the way from a final state to the start state building a word
(define (find-path predecessors state word graph)
  (if (hash-ref predecessors state)
    (find-path predecessors 
      (hash-ref predecessors state) 
      (append word 
        (list 
          (edge-weight graph
                       (hash-ref predecessors state) state)))
      graph)
    word))

;; function that build the words that can be accepted on the automaton
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

;; function that find the words that can be accepted on the automaton
(define (find-acceptance-words dfa)
  (define graph (weighted-graph/directed
    (transform-dfa-graph dfa)))
  (define-values (distance predecessors) 
    (bfs graph (dfa-start dfa)))
  (acceptance-words predecessors (dfa-final dfa) graph))

;; function that execute the correction algorithm idea
(define (automaton-correction answer feedback)
  (define dfa1 (automaton-difference answer feedback))
  (define dfa2 (automaton-difference feedback answer))
  (cond
    [(and 
      (not (empty? (dfa-final dfa1))) 
      (not (empty? (dfa-final dfa2))) 
        (displayln (string-append 
            (format "~a: ~a" "Your automaton is accepting a word(s) that it should not" (find-acceptance-words dfa1))
            "\n"
            (format "~a: ~a" "Your automaton is not accepting word(s) that it should" (find-acceptance-words dfa2)))))]
    [(not (empty? (dfa-final dfa1))) (format "~a: ~a" "Your automaton is accepting a word(s) that it should not" (find-acceptance-words dfa1))]
    [(not (empty? (dfa-final dfa2))) (format "~a: ~a" "Your automaton is not accepting word(s) that it should" (find-acceptance-words dfa2))]
    [else (displayln "Your automaton accept the same words of the feedback")]))
