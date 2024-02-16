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
  #;(gen:one-of (list (EMPTY) (LAMBDA) a b c um zero))
  #;(gen:one-of (list (EMPTY) (LAMBDA) um zero))
  (gen:one-of (list (LAMBDA) um zero))
  #;(gen:one-of (list um zero)))

#| valores iniciais
união: 40
interseção: 5
concatenação: 30
kleene: 15
complemento: 10
|#

#| valores atuais
união: 25
interseção: 5
concatenação: 40
kleene: 20
complemento: 10
|#
(define (gen:re h)
  (if (<= h 1)
      gen:symbol
      (let* ([h2 (sub1 h)])
        (gen:frequency `((25 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         #;(gen:const (union-regex r s))
                                         (gen:const (UNION r s))))
                         ( 5 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         #;(gen:const (inter-regex r s))
                                         (gen:const (INTERSECTION r s))))
                         (40 . ,(gen:let ([r (gen:re h2)]
                                          [s (gen:re h2)])
                                         #;(gen:const (concat-regex r s))
                                         (gen:const (CONCATENATION r s))))
                         (20 . ,(gen:let ([r (gen:re h2)])
                                         #;(gen:const (kleene-regex r))
                                         (gen:const (KLEENE-CLOSURE r))))
                         (10 . ,(gen:let ([r (gen:re h2)])
                                         #;(gen:const (complement-regex r))
                                         (gen:const (COMPLEMENT r)))))))))