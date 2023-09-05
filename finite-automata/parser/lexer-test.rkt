#lang racket

(require "lexer.rkt" parser-tools/lex)

(define (lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))


(define input
  "automato = {
    type = dfa
    start = A
    final = (B)
    sigma = (0 1)
    delta = (
        [C : 0 -> C]
    )
}")

(define (my-lex-test str)
    (lex-test (open-input-string str)))

(my-lex-test input)

(provide my-lex-test)