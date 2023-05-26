#lang racket

(require racket/contract)

(provide set-disjoint
         power-set
         big-union)


; extra functionalities for set functions

(define (set-disjoint s1 s2)
  (set-empty? (set-intersect s1 s2)))


;; power set functions

(define (power-set s)
  (define (incl y ys)
    (cons y ys))
  (match s
    ['() (list '())]
    [(cons x xs)
     (let ([ps (power-set xs)])
       (set-union
            ps
            (map (lambda (ys) (incl x ys)) ps)))]))


(define/contract (big-union ss)
  (list? . -> . list?)
  (remove-duplicates (flatten ss)))
