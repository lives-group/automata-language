#lang racket

(require "../fa.rkt"
         "../fa-image-builder.rkt"
         "../../utils/dot.rkt"
         "../../utils/set-extras.rkt"
         "../nfa/core.rkt"
         "core.rkt"
         racket/system
         racket/contract
         pict)

(provide (all-defined-out))

;; lambda closure function

(define/contract (fλ m E)
  (nfaλ? list? . -> . list?)
  (define delta (nfaλ-delta m))
  (define (lam-step e)
    (dict-ref delta
              (cons e 'λ)
              '()))
  (define E1 (flatten (append-map lam-step E)))
  (define X (remove-duplicates (append E E1)))
  (if (set=? X E)
      E
      (set-union X (fλ m X))))

;; converting to nfa

(define/contract (nfaλ->nfa M)
  (nfaλ? . -> . nfa?)
  (define I (nfaλ-start M))
  (define (sel p)
    (match p
      [(cons (cons e a) e1) (not (eq? a 'λ))]))
  (define delta (filter sel (nfaλ-delta M)))
  (define sigma (nfaλ-sigma M))
  (define (tr p)
    (match p
      [(cons (cons e a) e1) (cons (cons e a) (fλ M e1))]))
  (define M1 (mk-nfa (nfaλ-states M)
                     (nfaλ-sigma M)
                     (map tr delta)
                     (fλ M I)
                     (nfaλ-final M)))
  M1)
    
