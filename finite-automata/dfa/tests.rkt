#lang racket


(require "core.rkt"
         "image-builder.rkt"
         "../nfa/image-builder.rkt"
         "../nfa/subset-construction.rkt"
         "table-minimization.rkt"
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

;; (define reverse (revert-delta (dfa-delta A)))

;; (revert-delta (dfa-delta A))

(minimization T)

; function that group all the transitions that has the same beginning state and same symbol
(define (test delta)
  (remove-duplicates
   (map (lambda (t1)
         (filter (lambda (t2)
                (if (and (eqv? (car (car t1)) (car (car t2)))
                     (eqv? (cdr (car t1)) (cdr (car t2))))
                    (cons (cons (car (car t2)) (cdr (car t2))) (cdr t2))
                    #f
                    )) delta)) delta )))

; (test reverse)
; generate a list of final states of each grouped transition 
(define (solution grouped-transitions)
  (map (lambda (transitions)
         (map (lambda (single-transition)
                (cdr single-transition)) transitions)) grouped-transitions))

(define (concat-grouped-transitions grouped-transitions destiny-states)
  (map (lambda (group)
         (cons (car (first group)) (list (list-ref destiny-states (index-of grouped-transitions group))))) grouped-transitions))

;; (define s (solution (test reverse)))
;; (concat-grouped-transitions (test reverse) (solution (test reverse)) )
;(dfa->pict T)
;(dfa->pict (minimize T))
;(dfa->pict (minimization T))
