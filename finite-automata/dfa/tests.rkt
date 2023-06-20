#lang racket


(require "core.rkt"
         "image-builder.rkt"
         "../nfa/image-builder.rkt"
         "../nfa/subset-construction.rkt"
         "brzozowski-minimization.rkt"
         "../../utils/dot.rkt")


(define T
  (dfa A (B C) (A : 0 -> C)
       (A : 1 -> B)
       (B : 0 -> D)
       (B : 1 -> A)
       (C : 1 -> D)
       (C : 0 -> A)
       (D : 0 -> B)
       (D : 1 -> C)))

(define A
  (dfa A (B)
       (A : 0 -> B)
       (A : 1 -> B)
       (B : 0 -> B)
       (B : 1 -> B)))

(minimization T)