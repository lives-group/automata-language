#lang racket

(require brag)
(require parser-tools/yacc  "lexer.rkt")

;;; (define myparser
;;;   (parser
 
;;;    (start exp)
;;;    (end EOF)
;;;    (tokens op-tokens)
;;;    (src-pos)
;;;    (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   
   
;;;    (grammar
 
;;;     (exp  [(NUMBER) $1]
;;;           [(exp exp ADD) (+ $1 $2)]
;;;           [(exp exp SUBTRACT) (- $1 $2)]
;;;           [(exp exp PRODUCT) (* $1 $2)]
;;;           [(exp exp DIVISION) (/ $1 $2)]
;;;           [(exp exp POWER) (expt $1 $2)]
;;;           [(exp NEG) (- $1)]
;;;           ))))


brace-begin: /"{"
brace-end: /"}"
paren-begin: /"("
paren-end: /")"
bracket-begin: /"["
bracket-end: /"]"

automato-dfa: /"dfa"
automato-nfa: /"nfa"
automato-attribution: /"="
automato-destiny: /"->"
automato-symbol: /":"
automato-state: CHAR+ | STRING+
automato-word: INTEGER+ | CHAR+ | STRING+
automato-type-def: automato-dfa | automato-nfa
automato-start-def: CHAR+ | automato-start-many
automato-start-many: paren-begin CHAR+ paren-end
 
automato-description: automato-type (NEWLINE)* automato-start (NEWLINE)* automato-final (NEWLINE)* automato-sigma (NEWLINE)* automato-transition
automato-start: /"start" automato-attribution automato-start-def
automato-final: /"final" automato-attribution paren-begin CHAR+ paren-end
automato-sigma: /"sigma" automato-attribution paren-begin automato-state paren-end
automato-delta: /"delta" automato-attribution paren-begin automato-transition+ paren-end
automato-transiton: (brace-begin automato-state automato-symbol automato-word automato-destiny automato-state )+
automato-type: /"type" automato-attribution automato-type-def


automato: STRING automato-attribution brace-begin automato-description brace-end



