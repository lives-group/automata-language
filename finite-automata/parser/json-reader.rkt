#lang racket

(require json 
  "../dfa/automaton-correction.rkt"
  "../dfa/core.rkt"
  "../dfa/image-builder.rkt"
  "../fa.rkt"
  "../nfa/core.rkt"
  "../nfa/image-builder.rkt"
  "../nfa/subset-construction.rkt")

;; Open the JSON file for reading
(define input-file "finite-automata/parser/automato.json")
(define input-port (open-input-file input-file))

;; Read JSON data from the file
(define json-data (read-json input-port))

;; Close the input port after reading
(close-input-port input-port)

(define (transform-json-delta transitions)
  (map (lambda (t)
      (cons 
        (cons 
          (string->symbol (hash-ref t 'origin-state)) 
          (if (string? (hash-ref t 'symbol))
            (string->symbol (hash-ref t 'symbol))
            (hash-ref t 'symbol)))  
        (string->symbol (hash-ref t 'destiny-state)))) transitions))

(define (mk-automato-json json-data)
  (define states (map string->symbol (hash-ref json-data 'states)))
  (define start 
    (if (list? (hash-ref json-data 'start))
      (map string->symbol (hash-ref json-data 'start))
      (string->symbol (hash-ref json-data 'start))))
  (define final (map string->symbol (hash-ref json-data 'final)))
  (define sigma (hash-ref json-data 'sigma))
  (define delta (transform-json-delta (hash-ref json-data 'delta)))
  (if (string=? "dfa" (hash-ref json-data 'type))
    (mk-dfa states sigma delta start final)
    (mk-nfa states sigma delta start final)))

(define (mk-list-answer json-data)
  (map (lambda (automato)
    (mk-automato-json (hash-ref automato 'answer))) json-data))

(define (mk-list-feedback json-data)
  (map (lambda (automato)
    (mk-automato-json (hash-ref automato 'feedback))) json-data))


(mk-list-answer json-data)
(mk-list-feedback json-data)

