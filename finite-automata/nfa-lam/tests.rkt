#lang racket

(require "../fa.rkt"
         "../fa-image-builder.rkt"
         "../../utils/dot.rkt"
         "../../utils/set-extras.rkt"
         "core.rkt"
         "../nfa/image-builder.rkt"
         "../nfa/subset-construction.rkt"
         "../dfa/image-builder.rkt"
         "image-builder.rkt"
         "remove-lam.rkt"
         racket/system
         racket/contract
         pict)


(define M
  (nfaλ (A) (B C)
        (A : λ -> (B))
        (A : 1 -> (C))
        (B : 0 -> (D))
        (C : 1 -> (E))
        (D : 0 -> (B))
        (E : 1 -> (C))))

(nfaλ->pict M)

(nfa->pict (nfaλ->nfa M))

(dfa->pict (nfa->dfa (nfaλ->nfa M)))