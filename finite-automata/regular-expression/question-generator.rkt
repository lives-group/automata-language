#lang racket

(require rackcheck
         math/statistics
         "../dfa/core.rkt"
         "re.rkt"
         "re-generator.rkt"
         "re-to-dfa.rkt"
         "../dfa/image-builder.rkt"
         "re-to-nfa.rkt"
         "../nfa/image-builder.rkt")

(provide generate-questions
         MAX-RE-LENGTH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define um (SYMBOL #\1))
(define zero (SYMBOL #\0))
(define a (SYMBOL #\a))
(define b (SYMBOL #\b))
(define c (SYMBOL #\c))
(define d (SYMBOL #\d))

(define RE (UNION a (UNION b (UNION c d))))

(define RE1 (CONCATENATION a (CONCATENATION b (CONCATENATION c d))))

(define RE2 (CONCATENATION (KLEENE-CLOSURE (UNION zero um1)) (CONCATENATION um1 um1)))

(define RE3 (COMPLEMENT (CONCATENATION zero zero)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define MAX-GENERATIONS 10)

(define MAX-RE-LENGTH 5)

(define NUMBER-QUESTIONS 10)

(define SIGMA (list #\0 #\1))

;; Número de estados para questões fáceis, médias e difíceis
(define LIMIT-EASY 4)  ;; Fácil <= 4 estados
(define LIMIT-MEDIUM 8) ;; Médio <= 8 estados
(define LIMIT-HARD 12) ;; Difícil > 8 estados

(define MUTATION-RATE 0.01)
(define CROSSOVER-RATE 0.8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dfa-dot re)
  (pprint-re re)
  ((compose dfa->pict first dfa-rename-states (curry re-sigma-to-dfa SIGMA)) re))

(define (nfa-dot re)
  (pprint-re re)
  ((compose nfa->pict first nfa-rename-states re-to-nfa) re))

(define (re-height re)
  (match re
    [(EMPTY) 1]
    [(LAMBDA) 1]
    [(SYMBOL a) 1]
    [(CONCATENATION r s) (add1 (max (re-height r) (re-height s)))]
    [(KLEENE-CLOSURE r) (add1 (re-height r))]
    [(UNION r s) (add1 (max (re-height r) (re-height s)))]
    [(INTERSECTION r s) (add1 (max (re-height r) (re-height s)))]
    [(COMPLEMENT r) (add1 (re-height r))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (new-re max-length)
  (rewrite-re (first (sample (gen:re max-length) 1))))

(define (number-states re)
  (length (dfa-states (re-sigma-to-dfa SIGMA re))))

(define (number-transitions re)
  (length (dfa-delta (re-sigma-to-dfa SIGMA re))))

(define (razao-transicoes-estados re)
  (/ (length (dfa-delta (re-sigma-to-dfa SIGMA re))) (length (dfa-states (re-sigma-to-dfa SIGMA re)))))

(define (objective-easy re)
  (if (<= (number-states re) LIMIT-EASY) 0 1))

(define (objective-medium re)
  (if (<= (add1 LIMIT-EASY) (number-states re) LIMIT-MEDIUM) 0 1))

(define (objective-hard re)
  (if (<= (add1 LIMIT-MEDIUM) (number-states re) LIMIT-HARD) 0 1))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (fitnesses fitness-function population)
  (map fitness-function population))

(define (sort-by function r s)
  (< (function r) (function s)))

(define (population-fitness fitness-function population)
  (apply + (fitnesses fitness-function population)))

(define (best-individual population fitness-function)
  (first (sort population (curry sort-by fitness-function))))

(define (initial-population size [re-length MAX-RE-LENGTH])
  #;(map rewrite-re (sample (gen:re re-length) size))
  (sample (gen:re re-length) size))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (total-nodes re)
  (match re
    [(EMPTY) 0]
    [(LAMBDA) 0]
    [(SYMBOL a) 0]
    [(CONCATENATION r s) (add1 (total-nodes s))]
    [(KLEENE-CLOSURE r) (add1 (total-nodes r))]
    [(UNION r s) (add1 (total-nodes s))]
    [(INTERSECTION r s) (add1 (total-nodes s))]
    [(COMPLEMENT r) (add1 (total-nodes r))]))

(define (crossover-point re)
  (random (add1 (total-nodes re))))

(define (crossover-point2 re)
  (random (add1 (quotient (total-nodes re) 2))))

(define (get-subtree re point)
  (if (= point 0)
      re
      (match re
        [(EMPTY) re]
        [(LAMBDA) re]
        [(SYMBOL _) re]
        [(CONCATENATION r s) (get-subtree s (sub1 point))]
        [(UNION r s) (get-subtree s (sub1 point))]
        [(INTERSECTION r s) (get-subtree s (sub1 point))]
        [(KLEENE-CLOSURE r) (get-subtree r (sub1 point))]
        [(COMPLEMENT r) (get-subtree r (sub1 point))])))

(define (replace-subtree re new point)
  (if (= point 0)
      new
      (match re
        [(EMPTY) new]
        [(LAMBDA) new]
        [(SYMBOL _) new]
        [(CONCATENATION r s) (CONCATENATION r (replace-subtree s new (sub1 point)))]
        [(UNION r s) (UNION r (replace-subtree s new (sub1 point)))]
        [(INTERSECTION r s) (INTERSECTION r (replace-subtree s new (sub1 point)))]
        [(KLEENE-CLOSURE r) (KLEENE-CLOSURE (replace-subtree r new (sub1 point)))]
        [(COMPLEMENT r) (COMPLEMENT (replace-subtree r new (sub1 point)))])))

(define (crossover r s)
  (define point (crossover-point r))

  (define r-subtree (get-subtree r point))
  (define s-subtree (get-subtree s point))

  (define r2 (replace-subtree r s-subtree point))
  (define s2 (replace-subtree s r-subtree point))

  (list r2 s2))

(define (crossover2 r s)
  (define point1 (crossover-point2 r))
  (define point2 (random point1 (add1 (total-nodes r))))
  
  (define r-subtree1 (get-subtree r point1))
  (define r-subtree2 (get-subtree r point2))
  
  (define s-subtree1 (get-subtree s point1))
  (define s-subtree2 (get-subtree s point2))

  ; r s1 r2
  (define r2 (replace-subtree r (replace-subtree s-subtree1 r-subtree2 point2) point1))
  ; s r1 s2
  (define s2 (replace-subtree s (replace-subtree r-subtree1 s-subtree2 point2) point1))

  (list r2 s2))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (choose-path r s)
  (define smaller (min (re-height r) (re-height s)))
  (define length (random 1 (add1 smaller)))
  (build-list length (lambda (x) (random 2))))

(define (get-subtree-path re path)
  (if (empty? path)
      re
      (let* ([head (first path)]
             [tail (rest path)])
        (match re
          [(EMPTY) re]
          [(LAMBDA) re]
          [(SYMBOL _) re]
          [(CONCATENATION r s) (get-subtree-path (if (= head 0) r s) tail)]
          [(UNION r s) (get-subtree-path (if (= head 0) r s) tail)]
          [(INTERSECTION r s) (get-subtree-path (if (= head 0) r s) tail)]
          [(KLEENE-CLOSURE r) (get-subtree-path r tail)]
          [(COMPLEMENT r) (get-subtree-path r tail)]))))

#|
[(CONCATENATION r s) (if (= head 0)
                                   (get-subtree-path r tail)
                                   (get-subtree-path s tail))]
          [(UNION r s) (if (= head 0)
                           (get-subtree-path r tail)
                           (get-subtree-path s tail))]
          [(INTERSECTION r s) (if (= head 0)
                                  (get-subtree-path r tail)
                                  (get-subtree-path s tail))]
|#

(define (replace-subtree-path re new path)
  (if (empty? path)
      new
      (let* ([head (first path)]
             [tail (rest path)])
        (match re
          [(EMPTY) new]
          [(LAMBDA) new]
          [(SYMBOL _) new]
          [(CONCATENATION r s) (if (= head 0)
                                   (CONCATENATION (replace-subtree-path r new tail) s)
                                   (CONCATENATION r (replace-subtree-path s new tail)))]
          [(UNION r s) (if (= head 0)
                           (UNION (replace-subtree-path r new tail) s)
                           (UNION r (replace-subtree-path s new tail)))]
          [(INTERSECTION r s) (if (= head 0)
                                  (INTERSECTION (replace-subtree-path r new tail) s)
                                  (INTERSECTION r (replace-subtree-path s new tail)))]
          [(KLEENE-CLOSURE r) (replace-subtree-path r new tail)]
          [(COMPLEMENT r) (replace-subtree-path r new tail)]))))

(define (crossover-path r s)
  (define path (choose-path r s))

  (define r-subtree (get-subtree-path r path))
  (define s-subtree (get-subtree-path s path))

  (define r2 (replace-subtree-path r s-subtree path))
  (define s2 (replace-subtree-path s r-subtree path))

  (list r2 s2))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; Mutação
(define (mutate-subtree re point)
  (if (= point 0)
      (match re
        [(EMPTY) re]
        [(LAMBDA) re]
        [(SYMBOL _) re]
        [(CONCATENATION r s) (if (nullable? r)
                                 (if (< (random) 0.1) (UNION r s) (INTERSECTION r s))
                                 (if (< (random) 0.1) (INTERSECTION r s) (UNION r s)))]
        [(UNION r s) (if (< (random) 0.1) (INTERSECTION r s) (CONCATENATION r s)) ]
        [(INTERSECTION r s) (if (< (random) 0.1) (UNION r s) (CONCATENATION r s))]
        [(KLEENE-CLOSURE r) (if (= 0 (random 2)) (KLEENE-CLOSURE r) (COMPLEMENT r))]
        [(COMPLEMENT r) (if (= 0 (random 2)) (KLEENE-CLOSURE r) (COMPLEMENT r))])
      (match re
        [(EMPTY) re]
        [(LAMBDA) re]
        [(SYMBOL _) re]
        [(CONCATENATION r s) (CONCATENATION r (mutate-subtree s (sub1 point)))]
        [(UNION r s) (UNION r (mutate-subtree s (sub1 point)))]
        [(INTERSECTION r s) (INTERSECTION r (mutate-subtree s (sub1 point)))]
        [(KLEENE-CLOSURE r) (KLEENE-CLOSURE (mutate-subtree r (sub1 point)))]
        [(COMPLEMENT r) (COMPLEMENT (mutate-subtree r (sub1 point)))])))

(define (mutate-subtree-path re path)
  (if (empty? path)
      (match re
        [(EMPTY) re]
        [(LAMBDA) re]
        [(SYMBOL _) re]
        [(CONCATENATION r s) (if (nullable? r)
                                 (if (< (random) 0.1) (UNION r s) (INTERSECTION r s))
                                 (if (< (random) 0.1) (INTERSECTION r s) (UNION r s)))]
        [(UNION r s) (if (< (random) 0.1) (INTERSECTION r s) (CONCATENATION r s)) ]
        [(INTERSECTION r s) (if (< (random) 0.1) (UNION r s) (CONCATENATION r s))]
        [(KLEENE-CLOSURE r) (if (= 0 (random 2)) (KLEENE-CLOSURE r) (COMPLEMENT r))]
        [(COMPLEMENT r) (if (= 0 (random 2)) (KLEENE-CLOSURE r) (COMPLEMENT r))])
      (let* ([head (first path)]
             [tail (rest path)])
        (match re
          [(EMPTY) re]
          [(LAMBDA) re]
          [(SYMBOL _) re]
          [(CONCATENATION r s) (if (= head 0)
                                   (CONCATENATION (mutate-subtree-path r tail) s)
                                   (CONCATENATION r (mutate-subtree-path s tail)))]
          [(UNION r s) (if (= head 0)
                           (UNION (mutate-subtree-path r tail) s)
                           (UNION r (mutate-subtree-path s tail)))]
          [(INTERSECTION r s) (if (= head 0)
                                  (INTERSECTION (mutate-subtree-path r tail) s)
                                  (INTERSECTION r (mutate-subtree-path s tail)))]
          [(KLEENE-CLOSURE r) (KLEENE-CLOSURE (mutate-subtree-path r tail))]
          [(COMPLEMENT r) (COMPLEMENT (mutate-subtree-path r tail))]))))

(define (choose-path-mutate r)
  (define length (random 1 (add1 (re-height r))))
  (build-list length (lambda (x) (random 2))))

(define (mutate r)
  (define path (choose-path-mutate r))
  (mutate-subtree-path r path))

(define (mutate-population population)
  (map (lambda (x) (if (< (random) MUTATION-RATE) (mutate x) x)) population))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; método da roleta
(define (crossover-roleta population step)
  (define pai1 (list-ref population (random 0 (length population))))
  (define pai2 (list-ref population (random 0 (length population))))
  (define novo-par (if (< (random) CROSSOVER-RATE)
                       (crossover pai1 pai2)
                       (list pai1 pai2)))
  (define resto (remove pai2 (remove pai1 population)))
  (if (= step 0)
      (list)
      (append novo-par (crossover-roleta resto (sub1 step)))))

(define (gen-roleta population)
  (mutate-population (crossover-roleta population (quotient (length population) 2))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; método do torneio binário - pode repetir
(define (torneio-escolhe-pai population fitness-function n)
  (define positions (build-list n (lambda (x) (random (length population)))))
  (define candidatos (map (lambda (p) (list-ref population p)) positions))
  (best-individual candidatos fitness-function))

(define (crossover-torneio population step fitness-function n) 
  (define pai1 (torneio-escolhe-pai population fitness-function n))
  (define pai2 (torneio-escolhe-pai (remove pai1 population) fitness-function n))
  (define novo-par (if (< (random) CROSSOVER-RATE)
                       #;(crossover pai1 pai2)
                       #;(crossover2 pai1 pai2)
                       (crossover-path pai1 pai2)
                       (list pai1 pai2)))
  (if (= step 0)
      (list)
      (append novo-par (crossover-torneio population (sub1 step) fitness-function n))))

(define (gen-torneio population fitness-function [n 2])
  (mutate-population (crossover-torneio population (quotient (length population) 2) fitness-function n)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (new-population population fitness-function)
  #;(reproduct population fitness-function)
  #;(gen-roleta population)
  (gen-torneio population fitness-function))

(define (stop population fitness-function current-generation)
  (or (= (population-fitness fitness-function population) 0) (= current-generation MAX-GENERATIONS)))

(define (gen population fitness-function [current-generation 0] [return-generation #f])

  ;(display "Step: ") (display current-generation) #;(display population) (display "\n")
  ;(for-each pprint-re population)

  (if (stop population fitness-function current-generation)
      (if return-generation (list population current-generation) population)
      (gen (new-population population fitness-function) fitness-function (add1 current-generation) return-generation)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (generate-questions number-easy number-medium number-hard [re-length MAX-RE-LENGTH])
  #;(let* ([easy-questions (gen (initial-population NUMBER-QUESTIONS re-length) objective-easy 0)]
         [medium-questions (gen (initial-population NUMBER-QUESTIONS re-length) objective-medium 0)]
         [hard-questions (gen (initial-population NUMBER-QUESTIONS re-length) objective-hard 0)])
      (append (take easy-questions number-easy)
              (take medium-questions number-medium)
              (take hard-questions number-hard)))
  (append
    (build-list number-easy (lambda (x) (best-individual (gen (initial-population NUMBER-QUESTIONS re-length) objective-easy) objective-easy)))
    (build-list number-medium (lambda (x) (best-individual (gen (initial-population NUMBER-QUESTIONS re-length) objective-medium) objective-medium)))
    (build-list number-hard (lambda (x) (best-individual (gen (initial-population NUMBER-QUESTIONS re-length) objective-hard) objective-hard)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define (media-individuos population)
  (define states-population (map number-states population))
  (mean states-population))

(define (run-100-tests fitness-function)
  (define resultados (build-list 100
              (lambda (x)
                (let* ([result (gen (initial-population NUMBER-QUESTIONS 5) fitness-function 0 #t)])
                  ;(list (media-individuos (first result)) (second result))
                  (list (map number-states (first result)) (second result))
                  ))))

  ;(define estados (map first resultados))
  (define estados (flatten (map first resultados)))
  (define geracoes (map second resultados))

  (list
   (list (mean estados) (stddev estados))
   (list (mean geracoes) (stddev geracoes))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Testes

(define (test-crossover [r (new-re 5)] [s (new-re 5)])
  (define novas (crossover r s))

  (display "Primeira RE: ")
  (pprint-re r)
  (display "Segunda RE: ")
  (pprint-re s)

  (display "\nPrimeira RE filha: ")
  (pprint-re (first novas))
  (display "Segunda RE filha: ")
  (pprint-re (second novas)))

(define (test-crossover2 [r (new-re 5)] [s (new-re 5)])
  (define novas (crossover2 r s))

  (display "Primeira RE: ")
  (pprint-re r)
  (display "Segunda RE: ")
  (pprint-re s)


  (display "\nPrimeira RE filha: ")
  (pprint-re (first novas))
  (display "Segunda RE filha: ")
  (pprint-re (second novas)))

(define (test-mutate [r (new-re 5)])
  (define nova (mutate r))

  (display "RE:\r")
  (pprint-re r)

  (display "\nRE mutada:\n")
  (pprint-re nova))

(define (test-gen fitness-function [initial-population-size 10] [re-size 5] [return-generation #f])
  (let*
      ([result (gen (initial-population initial-population-size re-size) fitness-function 0 return-generation)])
    (map list
         (map (compose re->string rewrite-re) result)
         ;(map re->string result)
         (map number-states result)
         ;(map number-transitions result)
         ;(map razao-transicoes-estados result)
         #;(map dfa-dot result))))


(define (test-generate-questions n1 n2 n3 [re-size 5])
  (let*
      ([result (map rewrite-re (generate-questions n1 n2 n3 re-size))])
    (map list
         (map (compose re->string rewrite-re) result)
         #;(map re->string result)
         (map number-states result)
         ;(map number-transitions result)
         ;(map razao-transicoes-estados result)
         #;(map dfa-dot result))))