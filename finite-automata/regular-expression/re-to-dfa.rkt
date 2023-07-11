#lang racket

(require "re.rkt"
         "../dfa/core.rkt"
         "../fa.rkt")

;; Definição usadas nos testes

(define S (list #\0 #\1))
(define RE (CONCATENATION (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um)))

(define S1 (list #\a #\b #\c))
(define RE1 (UNION (CONCATENATION a b) (CONCATENATION a c)))

(define RE2 (CONCATENATION (KLEENE-CLOSURE (UNION zero um)) (CONCATENATION um um)))


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

(define (state-exists q graph)
  (ormap (curry equivalent? q) (fa-graph-states graph)))

;; Mutually recursive functions to create the automata

(define (mk-transition sigma q c graph)
  (define qc (derivative c q))
  (if (state-exists qc graph)
      (add-transition (list q c qc) graph)
      (let ([graph-n (add-transition (list q c qc) (add-state qc graph))])
        (mk-graph sigma graph-n qc))))

(define (mk-graph sigma graph q)
  (foldl (curry mk-transition sigma q)
         graph
         sigma))

;; Construct a DFA from a regular expression and alphabet

(define (mk-dfa2 states sigma delta start final)
   (fa 'dfa states sigma delta start final))

(define (re-sigma-to-dfa sigma re)
  (define start re)
  (define graph (mk-graph sigma (fa-graph (list start) '()) start))
  (define Q (fa-graph-states graph))
  (define d (fa-graph-transitions graph))
  (define F (filter nullable? Q))
  (mk-dfa2 Q sigma d start F))

;; get alphabet from a regular expression

(define (get-alphabet re)
  (match re
    [(EMPTY) (list)]
    [(LAMBDA) (list)]
    [(SYMBOL a) (list a)]
    [(CONCATENATION r s) (set-union (get-alphabet r) (get-alphabet s))]
    [(KLEENE-CLOSURE r) (get-alphabet r)]
    [(UNION r s) (set-union (get-alphabet r) (get-alphabet s))]
    [(INTERSECTION r s) (set-union (get-alphabet r) (get-alphabet s))]
    [(COMPLEMENT r) (get-alphabet r)]))

;; Construct a DFA from a regular expression

(define (re-to-dfa re)
  (re-sigma-to-dfa (get-alphabet re) re))


(module+ test
  (require rackunit)
  
  ;; test re-to-dfa
  
  (check-equal? (re-to-dfa (CONCATENATION (KLEENE-CLOSURE (UNION zero um)) (CONCATENATION um um)))
                (fa
                 'dfa
                 (list
                  (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                  (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1))
                  (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA)))
                 '(#\1 #\0)
                 (list
                  (list
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                   #\1
                   (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)))
                  (list
                   (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1))
                   #\1
                   (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA)))
                  (list
                   (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA))
                   #\1
                   (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA)))
                  (list
                   (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (list
                   (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (list
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))))
                 (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                 (list (UNION (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (SYMBOL #\1)) (LAMBDA)))))
  
  (check-equal? (re-to-dfa RE1)
                (fa
                 'dfa
                 (list (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) (EMPTY) (UNION (SYMBOL #\b) (SYMBOL #\c)) (LAMBDA))
                 '(#\c #\b #\a)
                 (list
                  (list (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\c (EMPTY))
                  (list (EMPTY) #\c (EMPTY))
                  (list (EMPTY) #\b (EMPTY))
                  (list (EMPTY) #\a (EMPTY))
                  (list (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\b (EMPTY))
                  (list (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\a (UNION (SYMBOL #\b) (SYMBOL #\c)))
                  (list (UNION (SYMBOL #\b) (SYMBOL #\c)) #\c (LAMBDA))
                  (list (LAMBDA) #\c (EMPTY))
                  (list (LAMBDA) #\b (EMPTY))
                  (list (LAMBDA) #\a (EMPTY))
                  (list (UNION (SYMBOL #\b) (SYMBOL #\c)) #\b (LAMBDA))
                  (list (UNION (SYMBOL #\b) (SYMBOL #\c)) #\a (EMPTY)))
                 (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c)))
                 (list (LAMBDA))))
  
  "All tests run")