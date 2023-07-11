#lang racket

(provide (all-defined-out))

;; Struct definitions

(struct EMPTY () #:transparent)
(struct LAMBDA () #:transparent)
(struct SYMBOL (a) #:transparent)
(struct CONCATENATION (res) #:transparent)
(struct KLEENE-CLOSURE (r) #:transparent)
(struct UNION (res) #:transparent)
(struct INTERSECTION (res) #:transparent)
(struct COMPLEMENT (r) #:transparent)

;; TODO: remover depois
(define um (SYMBOL #\1))
(define zero (SYMBOL #\0))
(define a (SYMBOL #\a))
(define b (SYMBOL #\b))
(define c (SYMBOL #\c))

(define RE (CONCATENATION (list (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um))))
(define RE1 (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c)))))
(define RE2 (CONCATENATION (list (KLEENE-CLOSURE (UNION (list zero um))) um um)))

;; Functions


;; Nullability 

(define (nullable? e)
  (match e
    [(EMPTY) #f]
    [(LAMBDA) #t]
    [(SYMBOL a) #f]
    [(CONCATENATION res) (andmap nullable? res)]
    [(KLEENE-CLOSURE r) #t]
    [(UNION res) (ormap nullable? res)]
    [(INTERSECTION res) (andmap nullable? res)]
    [(COMPLEMENT r) (not (nullable? r))]))

(define (v e)
  (if (nullable? e) (LAMBDA) (EMPTY)))

;; Helper functions for regex simplification

(define (union-regex r)
  (define s (remove* (list (EMPTY)) r))
  (match (length s)
    [0 (EMPTY)]
    [1 (first s)]
    [any (UNION s)]))
;; se tiver alguma expressão diferente de LAMBDA que seja anulável, remover qualquer LAMBDA que fizer parte da união

(define (inter-regex r)
  (if (member (EMPTY) r)
      (EMPTY)
      (INTERSECTION r)))
#;
(define (unnest-concat r)
  (match r
    [(list x) x]
    [(cons x xs) ()])
  )

(define (concat-regex r)
  (if (member (EMPTY) r)
      (EMPTY)
      (let ([s (remove* (list (LAMBDA)) r)])
        (match (length s)
          [0 (LAMBDA)]
          [1 (first s)]
          [any (CONCATENATION s)]))))

(define (concat-has-one r)
  (if (> (length r) 1)
      (CONCATENATION r)
      (first r)))

;; Derivada

(define (derivative a e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL b) (if (char=? a b) (LAMBDA) (EMPTY))]
    [(CONCATENATION res) (let* ([r (first res)]
                                [s (rest res)])
                           (union-regex
                            (list
                             (concat-regex (append (list (derivative a r)) s))
                             (concat-regex (append (list (v r)) (list (derivative a (concat-has-one s))))))))]
    [(KLEENE-CLOSURE r) (concat-regex (list (derivative a r) e))]
    [(UNION res) (union-regex (map (curry derivative a) res))]
    [(INTERSECTION res) (inter-regex (map (curry derivative a) res))]
    [(COMPLEMENT r) (COMPLEMENT (derivative a r))]))

;; Extended derivative

(define (string-derivative char-list regex)
  (match (cons char-list regex)
    [(cons (list) _) (nullable? regex)]
    [(cons _ (EMPTY)) #f]
    [any (string-derivative (rest char-list) (derivative (first char-list) regex))]))

;; Regex equivalence

(define (add-to-front s e)
  (append (list e) s))

(define (distribute u s)
  (map (curry add-to-front s) u))

(define (lin1 e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    [(UNION r) (union-regex (map lin1 r))]
    [(KLEENE-CLOSURE r) (concat-regex (list (lin1 r) e))]
    [(CONCATENATION res) (let* ([r (first res)]
                                [s (rest res)])
                           (match r
                             [(UNION res2) (union-regex (map lin1 (distribute res2 s)))]
                             [(KLEENE-CLOSURE res2) (union-regex
                                                     (append
                                                      (list (concat-regex
                                                             (append
                                                              (list (lin1 res2))
                                                              (list r)
                                                              s)))
                                                      (map lin1 s)))]
                             [any (concat-regex (append (list r) s))]))]))

(define (lin2 e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    [(UNION res) (union-regex (map lin2 res))]
    [(SYMBOL a) (SYMBOL a)]
    [(CONCATENATION res) (let* ([r (first res)]
                                [s (rest res)])
                           (match r
                             [(UNION res2) (union-regex (map lin2 (distribute res2 s)))]
                             [any (concat-regex (append (list r) s))]))]))

(define (lin e)
  (lin2 (lin1 e)))

;; TODO: terminar det
(define (det e)
  (match e
    [(EMPTY) (EMPTY)]
    [(LAMBDA) (EMPTY)]
    [(SYMBOL a) (SYMBOL a)]
    #;[]))

;; Tests

(module+ test
  (require rackunit)
  
  ;; test nullable?

  (check-false (nullable? (EMPTY)))
  (check-true (nullable? (LAMBDA)))
  (check-false (nullable? a))
  (check-true (nullable? (KLEENE-CLOSURE (CONCATENATION (list a b)))))
  (check-true (nullable? (UNION (list (CONCATENATION (list b (KLEENE-CLOSURE b))) (LAMBDA)))))
  (check-false (nullable? (INTERSECTION (list (CONCATENATION (list b (KLEENE-CLOSURE b))) (LAMBDA)))))
  (check-true (nullable? (COMPLEMENT (EMPTY))))
  (check-false (nullable? (COMPLEMENT (LAMBDA))))

  ;; test v
  
  (check-equal? (v (EMPTY)) (EMPTY))
  (check-equal? (v (LAMBDA)) (LAMBDA))
  (check-equal? (v a) (EMPTY))
  (check-equal? (v (KLEENE-CLOSURE (CONCATENATION (list a b)))) (LAMBDA))
  (check-equal? (v (UNION (list (CONCATENATION (list b (KLEENE-CLOSURE b))) (LAMBDA)))) (LAMBDA))
  (check-equal? (v (INTERSECTION (list (CONCATENATION (list b (KLEENE-CLOSURE b))) (LAMBDA)))) (EMPTY))
  (check-equal? (v (COMPLEMENT (EMPTY))) (LAMBDA))
  (check-equal? (v (COMPLEMENT (LAMBDA))) (EMPTY))

  ;; test union-regex

  (check-equal? (union-regex (list (EMPTY) a)) a)
  (check-equal? (union-regex (list (EMPTY) (CONCATENATION (list b b)))) (CONCATENATION (list b b)))
  (check-equal? (union-regex (list (UNION (list b a)) (EMPTY))) (UNION (list b a)))
  (check-equal? (union-regex (list a b)) (UNION (list a b)))
  (check-equal? (union-regex (list b a)) (UNION (list b a)))
  (check-equal? (union-regex (list (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b))))
                (UNION (list (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b)))))

  ;; test concat-regex
 
  (check-equal? (concat-regex (list (EMPTY) a)) (EMPTY))
  (check-equal? (concat-regex (list (EMPTY) (CONCATENATION (list b b)))) (EMPTY))
  (check-equal? (concat-regex (list (UNION (list b a)) (EMPTY))) (EMPTY))
  (check-equal? (concat-regex (list (LAMBDA) a)) a)
  (check-equal? (concat-regex (list (LAMBDA) (CONCATENATION (list b b)))) (CONCATENATION (list b b)))
  (check-equal? (concat-regex (list (UNION (list b a)) (LAMBDA))) (UNION (list b a)))

  ;; test inter-regex
 
  (check-equal? (inter-regex (list a b)) (INTERSECTION (list a b)))
  (check-equal? (inter-regex (list b a)) (INTERSECTION (list b a)))
  (check-equal? (inter-regex (list (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b))))
                (INTERSECTION (list (COMPLEMENT (KLEENE-CLOSURE a)) (KLEENE-CLOSURE (COMPLEMENT b)))))
  

  ;; test derivative
  
  (check-equal? (derivative #\a (UNION (list (KLEENE-CLOSURE (CONCATENATION (list a b))) (CONCATENATION (list a a)))))
                (UNION (list (CONCATENATION (list b (KLEENE-CLOSURE (CONCATENATION (list a b))))) a)))
  (check-equal? (derivative #\a a) (LAMBDA))
  (check-equal? (derivative #\a b) (EMPTY))
  (check-equal? (derivative #\a (CONCATENATION (list a b))) b)
  (check-equal? (derivative #\a (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c)))))
                (UNION (list b c)))
  (check-equal? (derivative #\b (derivative #\a (KLEENE-CLOSURE (CONCATENATION (list a b c)))))
                (CONCATENATION (list c (KLEENE-CLOSURE (CONCATENATION (list a b c))))))
  (check-equal? (derivative #\c (derivative #\b (derivative #\a (KLEENE-CLOSURE (CONCATENATION (list a b c))))))
                (KLEENE-CLOSURE (CONCATENATION (list a b c))))
  (check-equal? (derivative #\b (KLEENE-CLOSURE (CONCATENATION (list a b c))))
                (EMPTY))
  (check-equal? (derivative #\a (INTERSECTION (list (KLEENE-CLOSURE a) (CONCATENATION (list b c)))))
                (EMPTY))
  #;
  (check-equal? (derivative #\a (KLEENE-CLOSURE (CONCATENATION (list a a a))))
                (CONCATENATION (list a a (KLEENE-CLOSURE (CONCATENATION (list a a a))))))

  ;; test string-derivative

  (check-true (string-derivative (string->list "00111") (CONCATENATION (list (KLEENE-CLOSURE zero) (KLEENE-CLOSURE um)))))
  (check-false (string-derivative (string->list "00111") (CONCATENATION (list (KLEENE-CLOSURE um) (KLEENE-CLOSURE zero)))))
  (check-true (string-derivative (string->list "ab") (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c))))))
  (check-true (string-derivative (string->list "ac") (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c))))))
  (check-false (string-derivative (string->list "aa") (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c))))))
  (check-false (string-derivative (string->list "a") (UNION (list (CONCATENATION (list a b)) (CONCATENATION (list a c))))))
  (check-true (string-derivative (string->list "1111111111111") (KLEENE-CLOSURE um)))
  (check-true (string-derivative (string->list "") (KLEENE-CLOSURE um)))
  (check-false (string-derivative (string->list "0") (KLEENE-CLOSURE um)))
  
  "All tests run")