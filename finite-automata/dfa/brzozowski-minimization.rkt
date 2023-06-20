#lang racket

(provide minimization)

(require "../fa.rkt"
         "core.rkt"
         "../dfa/image-builder.rkt"
         "../nfa/image-builder.rkt"
         "../../utils/set-extras.rkt"
         "../nfa/subset-construction.rkt"
         "../nfa/core.rkt")

; revert a single transition
(define (revert-transition t)
  (match t
    [(cons (cons o s) t) (cons (cons t s) o)]))

;; combine transitions with same domain

(define (combine-domain delta)
  (hash->list
    (foldr (lambda (t ac) (if (hash-has-key? ac (car t))
                              (hash-set ac (car t) (list (cons (cdr t)
                                                         (hash-ref ac (car t)))))
                              (hash-set ac (car t) (list (cdr t)))))
           (make-immutable-hash)
           delta)))

;; revert all transitions of an automaton
(define (revert-delta delta)
    (combine-domain (map revert-transition delta)))

;; make a reverse of a dfa and turn it into a nfa
(define (revert-dfa dfa)
  (mk-nfa 
    (dfa-states dfa)
    (dfa-sigma dfa)
    (revert-delta
     (remove-duplicates (dfa-delta dfa)))
    (dfa-final dfa)
    (list (dfa-start dfa))))

(define (minimization fa)
  (define step1 (nfa->dfa (revert-dfa fa)))
  (nfa->dfa (revert-dfa step1)))