#lang racket

(require "re.rkt"
         "../nfa/core.rkt"
         "../fa.rkt")

(provide re-to-nfa
         re-sigma-to-nfa
         nfa-rename-states
         nfa-states->symbol)

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
  (define has-equivalent (member q (fa-graph-states graph) equivalent?))
  (if has-equivalent
      (first has-equivalent)
      #f))


;; Mutually recursive functions to create the automata

(define (mk-transition sigma q c qc graph)
  (if (state-exists qc graph)
      (add-transition (cons (cons q c) qc) graph)
      (let ([graph-n (add-transition (cons (cons q c) qc) (add-state qc graph))])
        (mk-graph sigma graph-n qc))))

(define (mk-all-transitions sigma q c graph)
  (define qc (map rewrite-re (partial-derivative c q)))
  (foldl (curry mk-transition sigma q c)
         graph
         qc))

(define (mk-graph sigma graph q)
  (foldl (curry mk-all-transitions sigma q)
         graph
         sigma))


;; Construct a nfa from a regular expression and alphabet

(define (re-sigma-to-nfa sigma re)
  (define start re)
  (define graph (mk-graph sigma (fa-graph (list start) '()) start))
  (define Q (fa-graph-states graph))
  (define d (fa-graph-transitions graph))
  (define F (filter nullable? Q))
  (mk-nfa Q sigma d (list start) F))


;; Construct a NFA from a regular expression

(define (re-to-nfa re)
  (define re1 (rewrite-re re))
  (re-sigma-to-nfa (get-alphabet re1) re1))


;; Renaming states

(define (hash-states states)
  (define table (make-hash))
  (define L (length states))
  (define names (build-list L (lambda (x) ((compose string->symbol string integer->char) (+ x 65)))))
  (map (curry hash-set! table) states names)
  table)


(define (nfa-rename-states nfa)
  (define Q     (nfa-states nfa))
  (define sigma (nfa-sigma nfa))
  (define d     (nfa-delta nfa))
  (define start (nfa-start nfa))
  (define F     (nfa-final nfa))
  
  (define table (hash-states Q))

  (define Q2 (map (curry hash-ref table) Q))
  (define d2 (map
              (lambda (t) (cons
                           (cons (hash-ref table (car (car t)))
                                 (cdr (car t)))
                           (hash-ref table (cdr t))))
              d))
  (define start2 (map (curry hash-ref table) start))
  (define F2 (map (curry hash-ref table) F))
  
  (define nfa2 (mk-nfa Q2 sigma d2 start2 F2))

  (list nfa2 table))

(define (nfa-states->symbol nfa)
  (define Q     (nfa-states nfa))
  (define sigma (nfa-sigma nfa))
  (define d     (nfa-delta nfa))
  (define start (nfa-start nfa))
  (define F     (nfa-final nfa))
  

  (define Q2 (map (compose string->symbol re->string) Q))
  (define d2 (map
              (lambda (t) (cons
                           (cons ((compose string->symbol re->string) (car (car t)))
                                 (cdr (car t)))
                           ((compose string->symbol re->string) (cdr t))))
              d))
  (define start2 (map (compose string->symbol re->string) start))
  (define F2 (map (compose string->symbol re->string) F))
  
  (mk-nfa Q2 sigma d2 start2 F2))


#;
(module+ test
  (require rackunit)
  
  ;; test re-to-nfa
  
  "All tests run")