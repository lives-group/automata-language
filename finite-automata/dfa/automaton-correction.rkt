#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         "image-builder.rkt"
         "table-minimization.rkt")


(define (automaton-difference dfa1 dfa2)
    (mk-intersection-dfa dfa1 (complement dfa2)))