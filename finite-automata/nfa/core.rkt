#lang racket

(require "../fa.rkt"
         "../../utils/set-extras.rkt"
         syntax/parse
         (for-syntax syntax/parse
                     racket))


(provide nfa?
         nfa
         nfa-states
         nfa-sigma
         nfa-delta
         nfa-start
         nfa-final
         mk-nfa)


;; testing if something is a nfa

(define (nfa? m)
  (eq? 'nfa (fa-type m)))

;; projections

(define (nfa-states m)
  (fa-states m))

(define (nfa-sigma m)
  (fa-sigma m))

(define (nfa-delta m)
  (fa-delta m))

(define (nfa-start m)
  (fa-start m))

(define (nfa-final m)
  (fa-final m))

;; macro for creating a nfa

(define-syntax (nfa stx)
  (syntax-parse stx #:datum-literals (: ->)
    [(_ (start:id ...) (end:id ...) [state:id : sym:expr -> (next:id ...)] ...)
     #:fail-when (not (null? (filter (lambda (e) (eq? 'λ e))
                                     (syntax->datum #'(sym ...)))))
                "This finite machine has lambda transitions. Declare it as nfaλ."
     #'(fa 'nfa
           (remove-duplicates (append (syntax->datum #'(state ...))
                                      (flatten (list (syntax->datum #'(next ...)) ...))
                                      (syntax->datum #'(start ...))))
           (remove-duplicates (syntax->datum #'(sym ...)))
           (list (cons (cons 'state 'sym) (list (syntax->datum #'(next ...)))) ...)
           (list 'start ...)
           (list 'end ...))]))


;; extended transition function

(define (step m e a)
  (flatten (dict-ref (nfa-delta m)
            (cons e a))))

(define (nfa-delta-star m e s)
  (define (delta-step E a)
    (append-map (lambda (x) (step m x a)) E))
  (cond
    [(null? e) '()]
    [(null? s)  e ]
    [else (nfa-delta-star
              m
              (delta-step e (car s))
              (cdr s))]))

;; acceptance test

(define (nfa-accept? m s)
  (not (set-disjoint (nfa-delta-star m (nfa-start m) s)
                     (nfa-final m))))

;; constructor

(define (mk-nfa states sigma delta start final)
   (fa 'nfa states sigma delta start final))
