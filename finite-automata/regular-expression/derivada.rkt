#lang racket

(provide (all-defined-out))

;; Struct definitions

(struct EMPTY () #:transparent)
(struct LAMBDA () #:transparent)
(struct SYMBOL (a) #:transparent)
(struct CONCATENATION (r s) #:transparent)
(struct KLEENE-CLOSURE (r) #:transparent)
(struct UNION (r s) #:transparent)
(struct INTERSECTION (r s) #:transparent)
(struct COMPLEMENT (r) #:transparent)

;; TODO: remover depois
(define um (SYMBOL #\1))
(define zero (SYMBOL #\0))
(define a (SYMBOL #\a))
(define b (SYMBOL #\b))
(define c (SYMBOL #\c))

;; Functions


;; Nullability 

(define (nullable? e)
  (match e
    [(EMPTY) #f]
    [(LAMBDA) #t]
    [(SYMBOL a) #f]
    [(CONCATENATION r s) (and (nullable? r) (nullable? s))]
    [(KLEENE-CLOSURE r) #t]
    [(UNION r s) (or (nullable? r) (nullable? s))]
    [(INTERSECTION r s) (and (nullable? r) (nullable? s))]
    [(COMPLEMENT r) (not (nullable? r))]))

(define (v e)
  (if (nullable? e) (LAMBDA) (EMPTY)))

;; Helper functions for regex simplification

(define (or-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) s]
    [(cons _ (EMPTY)) r]
    [any (UNION r s)]))

(define (inter-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) (EMPTY)]
    [(cons _ (EMPTY)) (EMPTY)]
    [any (INTERSECTION r s)]))

(define (and-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) (EMPTY)]
    [(cons _ (EMPTY)) (EMPTY)]
    [(cons (LAMBDA) _) s]
    [(cons _ (LAMBDA)) r]
    [any (CONCATENATION r s)]))

;; Derivada

(define (derivative a e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL b) (if (char=? a b) (LAMBDA) (EMPTY))]
    [(CONCATENATION r s) (or-regex 
                          (and-regex (derivative a r) s) 
                          (and-regex (v r) (derivative a s)))]
    [(KLEENE-CLOSURE r) (and-regex (derivative a r) e)]
    [(UNION r s) (or-regex (derivative a r) (derivative a s))]
    [(INTERSECTION r s) (inter-regex (derivative a r) (derivative a s))]
    [(COMPLEMENT r) (COMPLEMENT (derivative a r))]))

;; Extended derivative

(define (string-derivative char-list regex)
  (match (cons char-list regex)
    [(cons (list) _) (nullable? regex)]
    [(cons _ (EMPTY)) #f]
    [any (string-derivative (rest char-list) (derivative (first char-list) regex))]))

;; Regex equivalence

(define (equivalent? r s)
  (match (cons r s)
    [(cons (LAMBDA) (LAMBDA)) #t]
    [(cons (EMPTY) (EMPTY)) #t]
    [(cons (SYMBOL a) (SYMBOL b)) (char=? a b)]
    [(cons (CONCATENATION a b) (CONCATENATION c d)) (and (equivalent? a c) (equivalent? b d))]
    [(cons (KLEENE-CLOSURE a) (KLEENE-CLOSURE b)) (equivalent? a b)]
    [(cons (UNION a b) (UNION c d)) (or
                                     (and (equivalent? a c) (equivalent? b d))
                                     (and (equivalent? a d) (equivalent? b c)))]
    [(cons (INTERSECTION a b) (INTERSECTION c d)) (or
                                                   (and (equivalent? a c) (equivalent? b d))
                                                   (and (equivalent? a d) (equivalent? b c)))]
    [(cons (COMPLEMENT a) (COMPLEMENT b)) (equivalent? a b)]
    [any #f]))
  

;; Tests

(module+ test
  (require rackunit)
  
  ;; test nullable?

  (check-false (nullable? (EMPTY)))
  (check-true (nullable? (LAMBDA)))
  (check-false (nullable? a))
  (check-true (nullable? (KLEENE-CLOSURE (CONCATENATION a b))))
  (check-true (nullable? (UNION (CONCATENATION b (KLEENE-CLOSURE b)) (LAMBDA))))
  (check-false (nullable? (INTERSECTION (CONCATENATION b (KLEENE-CLOSURE b)) (LAMBDA))))
  (check-true (nullable? (COMPLEMENT (EMPTY))))
  (check-false (nullable? (COMPLEMENT (LAMBDA))))

  ;; test v
  
  (check-equal? (v (EMPTY)) (EMPTY))
  (check-equal? (v (LAMBDA)) (LAMBDA))
  (check-equal? (v a) (EMPTY))
  (check-equal? (v (KLEENE-CLOSURE (CONCATENATION a b))) (LAMBDA))
  (check-equal? (v (UNION (CONCATENATION b (KLEENE-CLOSURE b)) (LAMBDA))) (LAMBDA))
  (check-equal? (v (INTERSECTION (CONCATENATION b (KLEENE-CLOSURE b)) (LAMBDA))) (EMPTY))
  (check-equal? (v (COMPLEMENT (EMPTY))) (LAMBDA))
  (check-equal? (v (COMPLEMENT (LAMBDA))) (EMPTY))

  ;; test or-regex

  (check-equal? (or-regex (EMPTY) a) a)
  (check-equal? (or-regex (EMPTY) (CONCATENATION b b)) (CONCATENATION b b))
  (check-equal? (or-regex (UNION b a) (EMPTY)) (UNION b a))
  (check-equal? (or-regex a b) (UNION a b))
  (check-equal? (or-regex b a) (UNION b a))
  (check-equal? (or-regex (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b)))
                (UNION (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b))))

  ;; test and-regex

  (check-equal? (and-regex (EMPTY) a) (EMPTY))
  (check-equal? (and-regex (EMPTY) (CONCATENATION b b)) (EMPTY))
  (check-equal? (and-regex (UNION b a) (EMPTY)) (EMPTY))
  (check-equal? (and-regex (LAMBDA) a) a)
  (check-equal? (and-regex (LAMBDA) (CONCATENATION b b)) (CONCATENATION b b))
  (check-equal? (and-regex (UNION b a) (LAMBDA)) (UNION b a))

  ;; test inter-regex
  
  (check-equal? (inter-regex a b) (INTERSECTION a b))
  (check-equal? (inter-regex b a) (INTERSECTION b a))
  (check-equal? (inter-regex (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b)))
                (INTERSECTION (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b))))
  

  ;; test derivative
  
  (check-equal? (derivative #\a (UNION (KLEENE-CLOSURE (CONCATENATION a b)) (CONCATENATION a a)))
                (UNION (CONCATENATION b (KLEENE-CLOSURE (CONCATENATION a b))) a))
  (check-equal? (derivative #\a a) (LAMBDA))
  (check-equal? (derivative #\a b) (EMPTY))
  (check-equal? (derivative #\a (CONCATENATION a b)) b)
  (check-equal? (derivative #\a (UNION (CONCATENATION a b) (CONCATENATION a c)))
                (UNION b c))
  (check-equal? (derivative #\b (derivative #\a (KLEENE-CLOSURE (CONCATENATION a (CONCATENATION b c)))))
                (CONCATENATION c (KLEENE-CLOSURE (CONCATENATION a (CONCATENATION b c)))))
  (check-equal? (derivative #\c (derivative #\b (derivative #\a (KLEENE-CLOSURE (CONCATENATION a (CONCATENATION b c))))))
                (KLEENE-CLOSURE (CONCATENATION a (CONCATENATION b c))))
  (check-equal? (derivative #\b (KLEENE-CLOSURE (CONCATENATION a (CONCATENATION b c))))
                (EMPTY))
  (check-equal? (derivative #\a (INTERSECTION (KLEENE-CLOSURE a) (CONCATENATION b c)))
                (EMPTY))

  
  ;; test string-derivative

  (check-true (string-derivative (string->list "00111") (CONCATENATION (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um))))
  (check-false (string-derivative (string->list "00111") (CONCATENATION (KLEENE-CLOSURE um) (KLEENE-CLOSURE zero))))
  (check-true (string-derivative (string->list "ab") (UNION (CONCATENATION a b) (CONCATENATION a c))))
  (check-true (string-derivative (string->list "ac") (UNION (CONCATENATION a b) (CONCATENATION a c))))
  (check-false (string-derivative (string->list "aa") (UNION (CONCATENATION a b) (CONCATENATION a c))))
  (check-false (string-derivative (string->list "a") (UNION (CONCATENATION a b) (CONCATENATION a c))))
  (check-true (string-derivative (string->list "1111111111111") (KLEENE-CLOSURE um)))
  (check-true (string-derivative (string->list "") (KLEENE-CLOSURE um)))
  (check-false (string-derivative (string->list "0") (KLEENE-CLOSURE um)))

  "All tests run")