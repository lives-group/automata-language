#lang racket

(provide (all-defined-out))

;; Struct definitions

(struct LAMBDA () #:transparent)
(struct SYMBOL (a) #:transparent)
(struct VARIABLE (a) #:transparent)

(struct grammar
  (variables    
   alphabet
   rules        ;; [(VARIABLE, [SYMBOL or VARIABLE])]
   start)
  #:transparent)

;; Grammar for (0+1)*11
(define example-grammar
  (grammar
   '(S A B)  ;; variables
   '(|0| |1|)  ;; alphabet
   (list
    (cons (VARIABLE "S") (list (SYMBOL "0") (VARIABLE "S")))  ;; S -> 0 S
    (cons (VARIABLE "S") (list (SYMBOL "1") (VARIABLE "S")))  ;; S -> 1 S
    (cons (VARIABLE "S") (list (SYMBOL "1") (VARIABLE "A")))  ;; S -> 1 A
    (cons (VARIABLE "A") (list (SYMBOL "1") (VARIABLE "B")))  ;; A -> 1 B
    (cons (VARIABLE "B") (list (LAMBDA))))  ;; B -> ε
   #;(list
    (cons (VARIABLE "S") (list (SYMBOL "0") (VARIABLE "S")))  ;; S -> 0 S
    (cons (VARIABLE "S") (list (SYMBOL "1") (VARIABLE "A")))  ;; S -> 1 S
    (cons (VARIABLE "A") (list (SYMBOL "0") (VARIABLE "S")))  ;; S -> 0 S
    (cons (VARIABLE "A") (list (SYMBOL "1") (VARIABLE "B")))  ;; S -> 1 S
    (cons (VARIABLE "B") (list (SYMBOL "0") (VARIABLE "S")))  ;; S -> 0 S
    (cons (VARIABLE "B") (list (SYMBOL "1") (VARIABLE "B")))  ;; S -> 1 S
    (cons (VARIABLE "B") (list (LAMBDA))))  ;; B -> ε
   'S))  ;; start

;; Helper functions for regular-grammar?

(define (valid-start? start var)
  (member start var))

(define (valid-rule? rule variables alphabet)
  (define lhs (car rule))
  (define rhs (cdr rule))
  (define len (length rhs))
  
  (and (VARIABLE? lhs)
       (member ((compose string->symbol VARIABLE-a) lhs) variables)
       (cond [(= len 1)
               (or (and (SYMBOL? (first rhs))
                        (member ((compose string->symbol SYMBOL-a first) rhs) alphabet))
                   (LAMBDA? (first rhs)))]
             [(= len 2)
               (and (SYMBOL? (first rhs))
                    (member ((compose string->symbol SYMBOL-a first) rhs) alphabet)
                    (VARIABLE? (second rhs))
                    (member ((compose string->symbol VARIABLE-a second) rhs) variables))]
             [else #f])))

(define (regular-grammar? grammar)
  (define variables (grammar-variables grammar))
  (define alphabet (grammar-alphabet grammar))
  (define rules (grammar-rules grammar))
  (define start (grammar-start grammar))
  
  (and (valid-start? start variables)
       (for/and ([rule rules])
         (valid-rule? rule variables alphabet))))
