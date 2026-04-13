#lang racket

(require "grammar.rkt"
         "grammar-to-nfa.rkt"
         "grammar-generator.rkt"
         "../nfa/image-builder.rkt"
         "../nfa/subset-construction.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         rackcheck)

(define alpha '(|0| |1|))

(define (new-grammar max-var max-rule alphabet [min-var 1])
  (first (sample (gen:grammar max-var max-rule alphabet min-var) 1)))

(define (test-grammar [min-var 1] [max-var 8] [max-rule 10]) (new-grammar max-var max-rule alpha min-var))

(define (mk-pict grammar) ((compose nfa->pict grammar->nfa) grammar))

(define (mk-pict-g grammar)
  (list grammar
        ((compose nfa->pict grammar->nfa) grammar)
        ((compose dfa->pict nfa->dfa grammar->nfa) grammar)))

(define mk-pict-dfa (compose dfa->pict nfa->dfa grammar->nfa))

(define (mk-picts grammar)
  (list (mk-pict grammar) (mk-pict-dfa grammar)))