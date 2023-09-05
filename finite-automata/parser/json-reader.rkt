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

;; Transform transitions from json to the expected data structure
(define (transform-json-delta transitions)
  (map (lambda (t)
      (cons 
        (cons 
          (string->symbol (hash-ref t 'origin-state)) 
          (if (string? (hash-ref t 'symbol))
            (string->symbol (hash-ref t 'symbol))
            (hash-ref t 'symbol)))  
        (string->symbol (hash-ref t 'destiny-state)))) transitions))

;; Create an automaton based on the json read
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

;; Create a list of pairs that contains answer and feedback
(define (mk-pairs-answer json-data)
  (map (lambda (automato)
    (cons (cons 
        (mk-automato-json (hash-ref automato 'answer))
        (mk-automato-json (hash-ref automato 'feedback)))
      (hash-ref automato 'question))) json-data))

;; Run the correction algorithm to all answers in the json
(define (run-answers-batch json-data)
  (define pairs-answer (mk-pairs-answer json-data))
  (map (lambda (pair)
    (displayln 
      (string-append "\n" (cdr pair)))
    (automaton-correction 
        (car (car pair))
        (cdr (car pair)))) pairs-answer))

(run-answers-batch json-data)
