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

(define RE (CONCATENATION (KLEENE-CLOSURE (UNION zero um)) (CONCATENATION um um)))
(define RE1 (CONCATENATION (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um)))
(define RE2 (UNION (CONCATENATION a b) (CONCATENATION a c)))
(define RE3 (UNION (UNION um zero) (UNION a b)))
(define RE4 (UNION
             (UNION
              (CONCATENATION
               (CONCATENATION a b)
               (CONCATENATION c c))
              (UNION zero um))
             (UNION
              (INTERSECTION (KLEENE-CLOSURE (KLEENE-CLOSURE a))
                            (CONCATENATION a (KLEENE-CLOSURE a)))
              (INTERSECTION
               (INTERSECTION a b)
               (INTERSECTION b c)))))

(define RE5 (INTERSECTION
             (INTERSECTION c b)
             (INTERSECTION a b)))


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

(define (union-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) s]
    [(cons _ (EMPTY)) r]
    [(cons (LAMBDA) e) (if (nullable? e) e (UNION r s))]
    [(cons e (LAMBDA)) (if (nullable? e) e (UNION r s))]    
    [any (if (equivalent? r s)
             r
             (UNION r s))]))

(define (inter-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) (EMPTY)]
    [(cons _ (EMPTY)) (EMPTY)]
    [(cons (LAMBDA) e) (if (nullable? e) (LAMBDA) (EMPTY))]
    [(cons e (LAMBDA)) (if (nullable? e) (LAMBDA) (EMPTY))]
    #;[(cons (SYMBOL a) (SYMBOL b)) (if (char=? a b) (SYMBOL a) (EMPTY))]
    [any (if (equivalent? r s)
             r
             (INTERSECTION r s))]))

(define (concat-regex r s)
  (match (cons r s)
    [(cons (EMPTY) _) (EMPTY)]
    [(cons _ (EMPTY)) (EMPTY)]
    [(cons (LAMBDA) _) s]
    [(cons _ (LAMBDA)) r]
    [any (CONCATENATION r s)]))

(define (kleene-regex r)
  (match r
    [(EMPTY) (LAMBDA)]
    [(LAMBDA) (LAMBDA)]
    [(KLEENE-CLOSURE e) r]
    [any (KLEENE-CLOSURE r)]))

(define (complement-regex r)
  (match r
    [(COMPLEMENT e) e]
    [any (COMPLEMENT r)]))

;; Derivada

(define (derivative a e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL b) (if (char=? a b) (LAMBDA) (EMPTY))]
    [(CONCATENATION r s) (union-regex 
                          (concat-regex (derivative a r) s) 
                          (concat-regex (v r) (derivative a s)))]
    [(KLEENE-CLOSURE r) (concat-regex (derivative a r) e)]
    [(UNION r s) (union-regex (derivative a r) (derivative a s))]
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



;; Sistema de reescrita de regex

;; União
(define (union-to-list e)
  (match e
    [(UNION r s) (append (union-to-list r) (union-to-list s))]
    [any (list e)]))

(define (simplify-union r)
  (define s (remove* (list (EMPTY)) r))
  (match (length s)
    [0 (list (EMPTY))]
    [any s]))

(define (list-to-union l)
  (match l
    [(list) (EMPTY)]
    [(list r) r]
    [(list r s) (union-regex r s)]
    [(cons x xs) (union-regex x (list-to-union xs))]))

;; Interseção
(define (inter-to-list e)
  (match e
    [(INTERSECTION r s) (append (inter-to-list r) (inter-to-list s))]
    [any (list e)]))

(define (simplify-inter r)
  (if (member (EMPTY) r)
      (list (EMPTY))
      r))

(define (list-to-inter l)
  (match l
    [(list) (EMPTY)]
    [(list r) r]
    [(list r s) (inter-regex r s)]
    [(cons x xs) (inter-regex x (list-to-inter xs))]))

;; Concatenação
(define (concat-to-list e)
  (match e
    [(CONCATENATION r s) (append (concat-to-list r) (concat-to-list s))]
    [any (list e)]))

(define (simplify-concat r)
  (if (member (EMPTY) r)
      (list (EMPTY))
      (let ([s (remove* (list (LAMBDA)) r)])
        (match (length s)
          [0 (list (LAMBDA))]
          [any s]))))

(define (list-to-concat l)
  (match l
    [(list) (EMPTY)]
    [(list r) r]
    [(list r s) (concat-regex r s)]
    [(cons x xs) (concat-regex x (list-to-concat xs))]))


;; Como comparar regex?
(define (regex-smaller? x y)
  (match (cons x y)
    [(cons (EMPTY) _) #f]
    [(cons _ (EMPTY)) #t]

    [(cons (LAMBDA) _) #f]
    [(cons _ (LAMBDA)) #t]
    
    [(cons (SYMBOL a) (SYMBOL b)) (char<? a b)]
    [(cons (SYMBOL _) s) #t]

    [(cons (KLEENE-CLOSURE r) (KLEENE-CLOSURE s)) (regex-smaller? r s)]
    [(cons (KLEENE-CLOSURE r) _) #f]
    [(cons _ (KLEENE-CLOSURE r)) #t]

    [(cons (COMPLEMENT r) (COMPLEMENT s)) (regex-smaller? r s)]
    [(cons (COMPLEMENT r) _) #f]
    [(cons _ (COMPLEMENT r)) #t]

    [(cons (UNION r s) (UNION r1 s1)) (regex-smaller? r r1)]
    [(cons (UNION r s) _) #f]
    [(cons _ (UNION r s)) #t]

    [(cons (INTERSECTION r s) (INTERSECTION r1 s1)) (regex-smaller? r r1)]
    [(cons (INTERSECTION r s) _) #f]
    [(cons _ (INTERSECTION r s)) #t]

    [(cons (CONCATENATION r s) (CONCATENATION r1 s1)) (regex-smaller? r r1)]
    [(cons (CONCATENATION r s) _) #f]
    [(cons _ (CONCATENATION r s)) #t]))


;; Reescrevendo, aninhando para direita
(define (rewrite-re e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (LAMBDA)]
    [(SYMBOL a) (SYMBOL a)]
    [(CONCATENATION r s) (list-to-concat
                          (map rewrite-re
                                (concat-to-list e)))]
    [(UNION r s) (list-to-union
                  (sort
                   (map rewrite-re
                        (union-to-list e))
                   regex-smaller?))]
    [(INTERSECTION r s) (list-to-inter
                         (sort
                          (map rewrite-re
                               (inter-to-list e))
                          regex-smaller?))]
    [(KLEENE-CLOSURE r) (kleene-regex (rewrite-re r))]
    [(COMPLEMENT r) (complement-regex (rewrite-re r))]))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usando a ideia do artigo de equivalência de regex
(define (lin1 e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    [(UNION r s) (union-regex (lin1 r) (lin1 s))]
    [(INTERSECTION r s) (inter-regex (lin1 r) (lin1 s))]
    [(KLEENE-CLOSURE r) (concat-regex (lin1 r) e)]
    [(CONCATENATION r s) (match r
                           [(SYMBOL a) (concat-regex r s)]
                           [(UNION r2 s2) (union-regex
                                            (lin1 (concat-regex r2 s))
                                            (lin1 (concat-regex s2 s)))]
                           [(KLEENE-CLOSURE r2) (union-regex
                                                 (concat-regex
                                                  (lin1 r2)
                                                  (concat-regex r s))
                                                 (lin1 s))]
                           [any (concat-regex r s)])]
    [(COMPLEMENT r) (complement-regex r)]))


(define (lin2 e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    [(UNION r s) (union-regex (lin2 r) (lin2 s))]
    #;[(INTERSECTION r s) (inter-regex (lin2 r) (lin2 s))]
    [(CONCATENATION r s) (match r
                           [(UNION r2 s2) (union-regex
                                         (lin2 (concat-regex r2 s))
                                         (lin2 (concat-regex s2 s)))]
                           [any (concat-regex r s)])]
    [any e]))


(define (lin e)
  (lin2 (lin1 e)))

;; TODO: terminar det
(define (det e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    [(UNION r s) (union-regex (det r) (det s))]
    [any e]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; printando regex
(define (pprint-regex e)
  (match e
    [(EMPTY) (display "∅")]
    [(LAMBDA) (display "λ")]
    [(SYMBOL a) (display a)]
    [(CONCATENATION r s) (display "(")
                         (pprint-regex r) (pprint-regex s)
                         (display ")")]
    [(KLEENE-CLOSURE r) (display "(") (pprint-regex r) (display ")*")]
    [(COMPLEMENT r) (display "¬(") (pprint-regex r) (display ")")]
    [(UNION r s) (display "(")
                 (pprint-regex r) (display " + ") (pprint-regex s)
                 (display ")")]
    [(INTERSECTION r s) (display "(")
                        (pprint-regex r) (display " ∩ ") (pprint-regex s)
                        (display ")")]))








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

  ;; test union-regex

  (check-equal? (union-regex (EMPTY) a) a)
  (check-equal? (union-regex (EMPTY) (CONCATENATION b b)) (CONCATENATION b b))
  (check-equal? (union-regex (UNION b a) (EMPTY)) (UNION b a))
  (check-equal? (union-regex a b) (UNION a b))
  (check-equal? (union-regex b a) (UNION b a))
  (check-equal? (union-regex (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b)))
                (UNION (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b))))

  ;; test concat-regex

  (check-equal? (concat-regex (EMPTY) a) (EMPTY))
  (check-equal? (concat-regex (EMPTY) (CONCATENATION b b)) (EMPTY))
  (check-equal? (concat-regex (UNION b a) (EMPTY)) (EMPTY))
  (check-equal? (concat-regex (LAMBDA) a) a)
  (check-equal? (concat-regex (LAMBDA) (CONCATENATION b b)) (CONCATENATION b b))
  (check-equal? (concat-regex (UNION b a) (LAMBDA)) (UNION b a))

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