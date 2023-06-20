#lang racket

;; implementation of the DFA minimization
;; algorithm using a fixpoint computation

(provide refinement-table
         minimize
         minimization
         revert-dfa
         revert-delta)

(require "../fa.rkt"
         "core.rkt"
         "../dfa/image-builder.rkt"
         "../nfa/image-builder.rkt"
         "image-builder.rkt"
         "../../utils/set-extras.rkt"
         "../nfa/subset-construction.rkt"
         "../nfa/core.rkt")

;; generating the initial partition
(define (init-partition m)
  (define final (dfa-final m))
  (define states (dfa-states m))
  (define (memfinal v)
    (not (member v final)))
  (list (filter memfinal states) final))

;; checking if two states are equivalent

(define (equiv? part m s1 s2)
  ;; alphabet from DFA
  (define sig (dfa-sigma m))
  ;; transition function
  (define delta (dfa-delta m))
  ;; generating all possible triples
  ;; from the states and alphabet symbols
  (define to-test
    (map (lambda (s)
           (cons s1 (cons s2 s)))
         sig))
  (define (find-set s)
    (findf (lambda (x)
             (member s x))
           part))
  ;; checking if some triple is formed
  ;; by equivalent states
  (define (check-equiv t)
    (match t
      [(cons s1 (cons s2 s))
       (let ([x1 (dict-ref delta
                           (cons s1 s))]
             [x2 (dict-ref delta
                           (cons s2 s))])
         (eq? (find-set x1)
              (find-set x2)))]))
  ;; checking all triples formed by the
  ;; states and alphabet symbols.
  (andmap check-equiv to-test))

;; generating the next partition
;; refinement

(define (next-partition m current)
  (append-map
   (lambda (p)
     (match p
       [(cons s1 s2)
        (group-by
         (lambda (x) (equiv? current m s1 x))
         p)]))
   current))

;; stop condition

(define (stop? table)
  (set=? (car table)
         (cadr table)))

;; create the sucessive refinement list

(define (refine m current)
  (let* ([next (next-partition
                   m
                   (car current))]
         [tbl (cons next current)])
    (if (stop? tbl)
        tbl
        (refine m tbl))))

(define (refinement-table m)
  (define start (init-partition m))
  (refine m (list start)))

;; constructing the minimal DFA

(define (minimize m)
  ;; getting the last partition
  (define sts
    (car (refinement-table m)))
  ;; generating fresh names for states
  (define (fresh n)
    (string->symbol (format "s~a" n)))
  (define fresh-names
    (map (lambda (s n)
           (cons s (fresh n)))
         sts
         (build-list (length sts)
                     values)))
  ;; applying the renaming
  (define (rename s)
    (findf (lambda (p) (member s (car p)))
           fresh-names))
  ;; start state
  (define start
    (cdr (rename (dfa-start m))))
  ;; final states
  (define finals
    (map (lambda (s) (cdr (rename s)))
     (flatten
       (filter
        (lambda (s)
          (not (set-disjoint
                s
                (dfa-final m))))
        sts))))
  ;; building the transition function
  (define (add-trans p)
    (match p
      [(cons (cons s1 c) s2)
       (cons (cons (cdr (rename s1))
                   c)
             (cdr (rename s2)))]))
  (define delta
    (remove-duplicates
     (map add-trans
          (dfa-delta m))))
  ;; finally, building the minimal
  ;; dfa from its components
  (mk-dfa (map cdr fresh-names)
          (dfa-sigma m)
          delta
          start
          finals))
