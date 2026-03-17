#lang racket

(require "grammar.rkt"
         "../nfa/core.rkt"
         "../fa.rkt")

(provide grammar->nfa)


(define (rule->transition rule)
  (define lhs (car rule))
  (define rhs (cdr rule))
  (define len (length rhs))

  (define state ((compose string->symbol VARIABLE-a) lhs))
  
  (cond [(= len 1)
          (if (SYMBOL? (first rhs))
              (let ([symbol ((compose string->symbol SYMBOL-a first) rhs)])
                (cons (cons state symbol) (list 'accept)))
              '())]
        [(= len 2)
          (define symbol ((compose string->symbol SYMBOL-a first) rhs))
          (define result ((compose string->symbol VARIABLE-a second) rhs))
          (cons (cons state symbol) (list result))]
        [else '()]))

(define (accepting-states rule finals)
  (define lhs (car rule))
  (define rhs (cdr rule))
  (define len (length rhs))

  (define state ((compose string->symbol VARIABLE-a) lhs))
  
  (cond [(= len 1)
          (if (LAMBDA? (first rhs))
              (cons state finals)
              finals)]
        [else finals]))

(define (grammar->nfa grammar)
  (unless (regular-grammar? grammar)
    (raise-argument-error 'grammar-to-nfa "Regular grammar" grammar))
  
  (define variables (grammar-variables grammar))
  (define alphabet (grammar-alphabet grammar))
  (define rules (grammar-rules grammar))
  (define start (grammar-start grammar))

  (define transitions
    (filter (negate empty?) (map rule->transition rules)))
  (define finals (foldl accepting-states '() rules))

  (define accept-needed?
    (ormap (lambda (t) (member 'accept (rest t))) transitions))
  
  (mk-nfa
   (if accept-needed?
       (cons 'accept variables)
       variables)
   alphabet
   transitions
   (list start)
   (if accept-needed?
       (cons 'accept finals)
       finals)))