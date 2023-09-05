#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))


(define-empty-tokens op-tokens (
  EOF 
  attribution 
  type 
  description 
  paren 
  bracket 
  braces 
  symbol 
  transition 
  number 
  numeric 
  char
  name))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace) (return-without-pos (next-token input-port))]
   [(:or "exercise" "start" "type" "final" "sigma" "delta") (token-description)]
   [(:or "dfa" "nfa") (token-type)]
   [#\= (token-attribution)]
   [(:or "{" "}") (token-braces)]
   [(:or "(" ")") (token-paren)]
   [(:or "[" "]") (token-bracket)]
   [#\: (token-symbol)]
   ["->" (token-transition)]
   [numeric (token-numeric)]
   [any-char (token-char)]))
 

(provide op-tokens next-token)   





