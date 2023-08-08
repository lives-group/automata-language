#lang racket

(require "re.rkt"
         "../dfa/core.rkt"
         "../fa.rkt")

(provide re-to-dfa
         re-sigma-to-dfa
         dfa-rename-states)

;; Definição usadas nos testes

(define S (list #\0 #\1))
(define RE (CONCATENATION (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um1)))

(define S1 (list #\a #\b #\c))
(define RE1 (UNION (CONCATENATION a b) (CONCATENATION a c)))

(define RE2 (CONCATENATION (KLEENE-CLOSURE (UNION zero um1)) (CONCATENATION um1 um1)))


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
  (define qc (rewrite-re (derivative c q)))
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
  (define re1 (rewrite-re re))
  (re-sigma-to-dfa (get-alphabet re1) re1))


;; Renaming states

(define (hash-states states)
  (define table (make-hash))
  (define L (length states))
  (define names (build-list L (lambda (x) ((compose string->symbol string integer->char) (+ x 65)))))
  (map (curry hash-set! table) states names)
  table)


(define (dfa-rename-states dfa)
  (define Q     (dfa-states dfa))
  (define sigma (dfa-sigma dfa))
  (define d     (dfa-delta dfa))
  (define start (dfa-start dfa))
  (define F     (dfa-final dfa))
  
  (define table (hash-states Q))

  (define Q2 (map (curry hash-ref table) Q))
  (define d2 (map
              (lambda (t) (list
                           (hash-ref table (first t))
                           (second t)
                           (hash-ref table (third t))))
              d))
  (define start2 (hash-ref table start))
  (define F2 (map (curry hash-ref table) F))
  
  (define dfa2 (mk-dfa2 Q2 sigma d2 start2 F2))

  (list dfa2 table))



(module+ test
  (require rackunit)
  
  ;; test re-to-dfa
  
  (check-equal? (re-to-dfa (CONCATENATION (KLEENE-CLOSURE (UNION zero um1)) (CONCATENATION um1 um1)))
                (fa
                 'dfa
                 (list
                  (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                  (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))
                 '(#\1 #\0)
                 (list
                  (list
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                   #\1
                   (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))))
                  (list
                   (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                   #\1
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))
                  (list
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA)))
                   #\1
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))
                  (list
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA)))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (list
                   (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (list
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                   #\0
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))))
                 (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                 (list (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))))
  
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