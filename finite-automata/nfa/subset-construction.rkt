#lang racket

(require "core.rkt"
         "../fa.rkt"
         "../dfa/core.rkt"
         "../dfa/image-builder.rkt"
         "../../utils/set-extras.rkt"
         racket/contract)


(provide subset-construction
         nfa->dfa)

;; definition of the subset construction

(struct delta-triple
  (origin symbol target)
  #:transparent)

(define/contract (subset-construction nfa)
  (nfa? . -> . dfa?)
  (define states (power-set (nfa-states nfa)))
  (define sig (nfa-sigma nfa))
  (define delta (nfa-delta nfa))
  (define nfin (nfa-final nfa))
  (define finals
    (filter (lambda (S)
              (not (set-disjoint S nfin)))
            states))
  (define (step s c)
    (dict-ref delta (cons s c) '()))
  (define (deltap X a)
    (big-union
     (append-map (lambda (s) (step s a)) X)))
  (define trans
    (append-map (lambda (S)
                  (map (lambda (a)
                         (delta-triple S a
                            (deltap S a))) sig))
                states))
  (define (mk-trans t)
    (match t
      [(delta-triple ors sym tar)
       (cons (cons ors sym) tar)]))
  (mk-dfa states
          sig
          (map mk-trans trans)
          (nfa-start nfa)
          finals))

(define/contract (nfa->dfa m)
  (nfa? . -> . dfa?)
  (renaming (reachable (renaming (subset-construction m)))))
