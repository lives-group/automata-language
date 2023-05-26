#lang racket

(require "../fa.rkt"
         "../../utils/set-extras.rkt"
         syntax/parse
         (for-syntax syntax/parse
                     racket))

(provide nfaλ-states
         nfaλ-sigma
         nfaλ-delta
         nfaλ-start
         nfaλ-final
         nfaλ
         nfaλ?
         nfaλ-delta-star
         nfaλ-accept?)


(define (nfaλ? m)
  (eq? 'nfaλ (fa-type m)))

;; projections

(define (nfaλ-states m)
  (fa-states m))

(define (nfaλ-sigma m)
  (fa-sigma m))

(define (nfaλ-delta m)
  (fa-delta m))

(define (nfaλ-start m)
  (fa-start m))

(define (nfaλ-final m)
  (fa-final m))

;; macro for creating a nfaλ

(define-syntax (nfaλ stx)
  (syntax-parse stx #:datum-literals (: ->)
    [(_ (start:id ...) (end:id ...) [state:id : sym:expr -> (next:id ...)] ...)
     #'(fa 'nfaλ
           (remove-duplicates (append (syntax->datum #'(state ...))
                                      (flatten (list (syntax->datum #'(next ...)) ...))
                                      (syntax->datum #'(start ...))))
           (set-remove (remove-duplicates (syntax->datum #'(sym ...))) 'λ)
           (list (cons (cons 'state 'sym) (list (syntax->datum #'(next ...)))) ...)
           (list 'start ...)
           (list 'end ...))]))

;; extended transition function

(define (nfaλ-delta-star m e s)
  (define (step x a)
    (set-union (dict-ref (nfaλ-delta m)
                         (cons e a))
               (dict-ref (nfaλ-delta m)
                         (cons e 'λ)
                         '())))
  (define (delta-step E a)
    (append-map (λ (s) (step s a)) E))
  (cond
    [(null? e) '()]
    [(null? s)  e ]
    [else (nfaλ-delta-star m
                           (delta-step e (car s))
                           (cdr s))]))

;; acceptance test

(define (nfaλ-accept? m s)
  (not (set-disjoint (nfaλ-delta-star m (nfaλ-start m) s)
                     (nfaλ-final m))))

;; constructor

(define (mk-nfaλ states sigma delta start final)
   (fa 'nfaλ states sigma delta start final))
