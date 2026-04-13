#lang racket

(require "grammar.rkt"
         rackcheck)

(provide gen:grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (possible-vars quant)
  (build-list quant (lambda (x) (string->symbol (string-append "V" (number->string x))))))

(define (gen:variables max-var [min-var 1])
  (gen:let ([num-vars (gen:integer-in min-var max-var)])
           (gen:const (take (possible-vars max-var) num-vars))))



(define (gen:transition-rules reached unreached rules alphabet)
  (if (empty? unreached)
      rules
      (gen:let ([origin (gen:one-of reached)]
                [term   (gen:one-of alphabet)]
                [rules-value rules]) ;; rules é um gerador, então precisa extrair o valor
               (let* ([target (first unreached)]
                      [remaining (rest unreached)]
                      [new-rule (cons (VARIABLE (symbol->string origin))
                                      (list (SYMBOL (symbol->string term))
                                            (VARIABLE (symbol->string target))))])
                 (gen:transition-rules (cons target reached)
                                       remaining
                                       (gen:const (cons new-rule rules-value))
                                       alphabet)))))

(define (gen:transitions variables alphabet)
  (gen:transition-rules
   (list (first variables))
   (rest variables)
   (gen:const '())
   alphabet))



(define (gen:terminal-rule var alphabet)
  (gen:let ([sym (gen:one-of alphabet)])
           (gen:const (cons (VARIABLE (symbol->string var))
                            (list (SYMBOL (symbol->string sym)))))))

(define (gen:terminal-rules variables alphabet)
  (if (empty? variables)
      (gen:const '())
      (let* ([var (first variables)]
             [remaining (rest variables)])
        (gen:let ([rule (gen:terminal-rule var alphabet)]
                  [rest (gen:terminal-rules remaining alphabet)])
                 (gen:const (cons rule rest))))))



(define (gen:with-frequency probability)
  (gen:frequency `((,probability . ,(gen:const #t))
                   (,(- 100 probability) . ,(gen:const #f)))))

(define (gen:lambda-rules variables alphabet)
  (if (empty? variables)
      (gen:const '())
      (let* ([var (first variables)]
             [remaining (rest variables)])
        (gen:let ([nullable? (gen:with-frequency 25)]
                  [rule  (gen:const (cons (VARIABLE (symbol->string var))
                                          (list (LAMBDA))))]
                  [rest (gen:lambda-rules remaining alphabet)])
                 (if nullable?
                     (gen:const (cons rule rest))
                     rest)))))

(define (gen:random-rule variables alphabet)
  (gen:let ([lhs (gen:one-of variables)]
            [sym (gen:one-of alphabet)] 
            [var (gen:one-of variables)])
           (gen:const (cons (VARIABLE (symbol->string lhs))
                            (list (SYMBOL (symbol->string sym))
                                  (VARIABLE (symbol->string var)))))))

(define (gen:random-rules variables alphabet num-rules)
  (if (= num-rules 0)
      (gen:const '())
      (gen:let ([rule  (gen:random-rule variables alphabet)]
                [rest  (gen:random-rules variables alphabet (- num-rules 1))])
               (gen:const (cons rule rest)))))

(define (gen:grammar max-var max-rule alphabet [min-var 1])
  (gen:let ([variables (gen:variables max-var min-var)]
            [num-rules (gen:integer-in 1 max-rule)]
            [transitions (gen:transitions variables alphabet)]
            [terminals (gen:terminal-rules variables alphabet)]
            [lambdas (gen:lambda-rules variables alphabet)]
            [random (gen:random-rules variables alphabet num-rules)])
           (let ([start (first variables)]
                 [rules (remove-duplicates (append transitions terminals lambdas random))])
             (gen:const (grammar variables alphabet rules start)))))
