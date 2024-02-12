#lang racket

(require "re.rkt"
         "../dfa/core.rkt"
         "../fa.rkt")

(provide re-to-dfa
         re-sigma-to-dfa
         dfa-rename-states
         dfa-states->symbol)

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
#;
(define (state-exists q graph)
  (ormap (curry equivalent? q) (fa-graph-states graph)))

(define (state-exists q graph)
  (define has-equivalent (member q (fa-graph-states graph) equivalent?))
  (if has-equivalent
      (first has-equivalent)
      #f))


;; Mutually recursive functions to create the automata

(define (mk-transition sigma q c graph)
  (define qc (rewrite-re (derivative c q)))
  (define equivalent-qc (state-exists qc graph))
  (if equivalent-qc
      (add-transition (cons (cons q c) equivalent-qc) graph)
      (let ([graph-n (add-transition (cons (cons q c) qc) (add-state qc graph))])
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
              (lambda (t) (cons
                           (cons (hash-ref table (car (car t)))
                                 (cdr (car t)))
                           (hash-ref table (cdr t))))
              d))
  (define start2 (hash-ref table start))
  (define F2 (map (curry hash-ref table) F))
  
  (define dfa2 (mk-dfa2 Q2 sigma d2 start2 F2))

  (list dfa2 table))

(define (dfa-states->symbol dfa)
  (define Q     (dfa-states dfa))
  (define sigma (dfa-sigma dfa))
  (define d     (dfa-delta dfa))
  (define start (dfa-start dfa))
  (define F     (dfa-final dfa))
  

  (define Q2 (map (compose string->symbol re->string) Q))
  (define d2 (map
              (lambda (t) (cons
                           (cons ((compose string->symbol re->string) (car (car t)))
                                 (cdr (car t)))
                           ((compose string->symbol re->string) (cdr t))))
              d))
  (define start2 ((compose string->symbol re->string) start))
  (define F2 (map (compose string->symbol re->string) F))
  
  (mk-dfa2 Q2 sigma d2 start2 F2))



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
                  (cons
                   (cons (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) #\1)
                   (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))))
                  (cons
                   (cons (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))) #\1)
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))
                  (cons
                   (cons
                    (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA)))
                    #\1)
                   (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))
                  (cons
                   (cons
                    (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA)))
                    #\0)
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (cons
                   (cons (UNION (SYMBOL #\1) (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))) #\0)
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))))
                  (cons
                   (cons (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) #\0)
                   (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))))
                 (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1)))
                 (list (UNION (SYMBOL #\1) (UNION (CONCATENATION (KLEENE-CLOSURE (UNION (SYMBOL #\0) (SYMBOL #\1))) (CONCATENATION (SYMBOL #\1) (SYMBOL #\1))) (LAMBDA))))))
  
  (check-equal? (re-to-dfa RE1)
                (fa
                 'dfa
                 (list (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) (EMPTY) (UNION (SYMBOL #\b) (SYMBOL #\c)) (LAMBDA))
                 '(#\c #\b #\a)
                 (list
                  (cons (cons (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\c) (EMPTY))
                  (cons (cons (EMPTY) #\c) (EMPTY))
                  (cons (cons (EMPTY) #\b) (EMPTY))
                  (cons (cons (EMPTY) #\a) (EMPTY))
                  (cons (cons (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\b) (EMPTY))
                  (cons (cons (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c))) #\a) (UNION (SYMBOL #\b) (SYMBOL #\c)))
                  (cons (cons (UNION (SYMBOL #\b) (SYMBOL #\c)) #\c) (LAMBDA))
                  (cons (cons (LAMBDA) #\c) (EMPTY))
                  (cons (cons (LAMBDA) #\b) (EMPTY))
                  (cons (cons (LAMBDA) #\a) (EMPTY))
                  (cons (cons (UNION (SYMBOL #\b) (SYMBOL #\c)) #\b) (LAMBDA))
                  (cons (cons (UNION (SYMBOL #\b) (SYMBOL #\c)) #\a) (EMPTY)))
                 (UNION (CONCATENATION (SYMBOL #\a) (SYMBOL #\b)) (CONCATENATION (SYMBOL #\a) (SYMBOL #\c)))
                 (list (LAMBDA))))
  
  "All tests run")