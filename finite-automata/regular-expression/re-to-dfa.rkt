#lang racket

(require "derivada.rkt"
         "../dfa/core.rkt")

;; Definição usadas nos testes

(define S (list #\0 #\1))
(define RE (CONCATENATION (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um)))

(define S1 (list #\a #\b #\c))
(define RE1 (UNION (CONCATENATION a b) (CONCATENATION a c)))


;; Struct representing the graph constructed

(struct fa-graph (states transitions) #:transparent)

;; Helper functions for graph manipulation

(define (add-state q graph)
  (let ([Q (fa-graph-states graph)]
        [d (fa-graph-transitions graph)])
    (fa-graph (append Q (list q)) d)))

(define (add-transition t graph)
  (let ([Q (fa-graph-states graph)]
        [d (fa-graph-transitions graph)])
    (fa-graph Q (append d (list t)))))

(define (my-or a)
  (match a
    [(list) #f]
    [(cons x xs) (or x (my-or xs))]))

(define (state-exists q graph)
  (my-or (map (curry equivalent? q) (fa-graph-states graph))))

;; Mutually recursive functions to create the automata

(define (mk-transition sigma q c graph)
  (define qc (derivative c q))
  (if (state-exists qc graph)
      (add-transition (list q c qc) graph)
      (let ([graph-n (add-transition (list q c qc) (add-state qc graph))])
        (mk-graph sigma graph-n qc)))
  )

(define (mk-graph sigma graph q)
  (foldl (curry mk-transition sigma q)
         graph
         sigma))

;; Construct a DFA from a regular expression and alphabet

(define (re-to-dfa sigma re)
  (define start re)
  (define graph (mk-graph sigma (fa-graph (list start) '()) start))
  (define Q (fa-graph-states graph))
  (define d (fa-graph-transitions graph))
  (define F (filter nullable? Q))
  ;;(mk-dfa Q sigma d start F))
  (values Q sigma d start F))