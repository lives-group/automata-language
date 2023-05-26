#lang racket


;; type definition for finite automata

(provide (all-defined-out))

(struct fa
  (type      ;; kind of finite state machine: dfa, nfa or nfaλ
   states    ;; state set
   sigma     ;; alphabet
   delta     ;; transition function
   start     ;; initial states
   final)    ;; final states
  #:transparent)

;; calcutating a renaming substitution for states

(define (ren-state sub s)
  (match sub
    ['() s]
    [(cons x xs)
     (cond
       [(eq? (car x) s) (cdr x)]
       [(symbol? (car x)) (ren-state xs s)]
       [(set=? (car x) s) (cdr x)]
       [else (ren-state xs s)])]))

(define (ren-states sub s)
  (map (lambda (x) (ren-state sub x)) s))

(define (renaming f)
  (define sts (fa-states f))
  (define (fresh n)
    (string->symbol (format "S~a" n)))
  (define (gen-sub xs ac)
    (match xs
      ['() '()]
      [(cons y ys) (cons (cons y (fresh ac))
                         (gen-sub ys (add1 ac)))]))
  (define sub (gen-sub sts 0))
  (define (ren-trans d)
    (match d
      [(cons (cons e simb) e1)
       (cons (cons (ren-state sub e) simb)
             (ren-state sub e1))]))
  (define (ren-delta ds)
    (map ren-trans ds))
  (fa (fa-type f)
      (ren-states sub sts)
      (fa-sigma f)
      (ren-delta (fa-delta f))
      (ren-state sub (fa-start f))
      (ren-states sub (fa-final f))))

;; calculating the set of reachable states

(define (one-step d s)
  (define (sel p)
    (match p
      [(cons (cons s1 c) s2) (eq? s1 s)]))
  (map (lambda (p) (cdr p)) (filter sel d)))

(define (steps d s ac)
  (define next (set-union (one-step d s)
                          ac))
  (cond
    [(set=? ac next) next]
    [else (let ([diff (set-subtract next ac)])
            (remove-duplicates
             (append-map
              (lambda (x) (steps d x next))
              diff)))]))

(define (reachable m)
  (define i (fa-start m))
  (define r-states (steps (fa-delta m) i (list i)))
  (define r-delta
    (filter (lambda (p) (member (car (car p))
                                r-states))
            (fa-delta m)))
  (define r-final (filter (lambda (p) (member p r-states))
                          (fa-final m)))
  (fa (fa-type m)
      r-states
      (fa-sigma m)
      r-delta
      i
      r-final))
    