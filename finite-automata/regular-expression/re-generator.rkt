#lang racket

(require "re.rkt"
         rackcheck)

(provide gen:re)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define um (SYMBOL #\1))
(define zero (SYMBOL #\0))
(define a (SYMBOL #\a))
(define b (SYMBOL #\b))
(define c (SYMBOL #\c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gen:symbol
  (gen:one-of (list (EMPTY) (LAMBDA) a b c um zero)))

(define (gen:re h)
  (if (<= h 1)
      gen:symbol
      (let* ([h2 (quotient h 2)])
        (gen:frequency `((40 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         (gen:const (union-regex r s))))
                         ( 5 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         (gen:const (inter-regex r s))))
                         (30 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         (gen:const (concat-regex r s))))
                         (15 . ,(gen:let ([r (gen:re h2)])
                                         (gen:const (kleene-regex r))))
                         (10 . ,(gen:let ([r (gen:re h2)])
                                         (gen:const (complement-regex r)))))))))