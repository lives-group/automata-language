#lang racket

(require json
         latex-utils/scribble/unmap
         latex-utils/scribble/math
         latex-utils/scribble/utils
         "../dfa/core.rkt"
         "re.rkt"
         "re-generator.rkt"
         "re-to-dfa.rkt"
         "../dfa/image-builder.rkt"
         "re-to-nfa.rkt"
         "../nfa/image-builder.rkt"
         "question-generator.rkt")

#;(provide generate-json
         generate-notebook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transition->jsexpr transition)
  (hasheq 'state ((compose car car) transition)
          'symbol ((compose cdr car) transition)
          'state2 (cdr transition)))

(define (dfa->jsexpr fa)
  (hasheq 'states (dfa-states fa)
          'sigma (dfa-sigma fa)
          'delta (map transition->jsexpr (dfa-delta fa))
          'start (dfa-start fa)
          'final (dfa-final fa)))

(define (re->jsexpr re)
  (match re
    [(EMPTY) (hasheq 'EMPTY "EMPTY")]
    [(LAMBDA) (hasheq 'LAMBDA "LAMBDA")]
    [(SYMBOL a) (hasheq 'SYMBOL (string a))]
    [(CONCATENATION r s) (hasheq 'CONCATENATION (list (re->jsexpr r) (re->jsexpr s)))]
    [(KLEENE-CLOSURE r) (hasheq 'KLEENE-CLOSURE (re->jsexpr r))]
    [(UNION r s) (hasheq 'UNION (list (re->jsexpr r) (re->jsexpr s)))]
    [(INTERSECTION r s) (hasheq 'INTERSECTION (list (re->jsexpr r) (re->jsexpr s)))]
    [(COMPLEMENT r) (hasheq 'COMPLEMENT (re->jsexpr r))]))

(define (re->json re)
  ;(define automata ((compose dfa-rename-states re-to-dfa) re))
  (hasheq 're re
          're-json (re->jsexpr (rewrite-re re))
          'string (re->string (rewrite-re re))
          ;'fda (dfa->jsexpr (first automata))
          ;'state-table (second automata)
          ))

(define (re-list->jsexpr number-easy number-medium number-hard [re-length MAX-RE-LENGTH])
  (define questions (generate-questions number-easy number-medium number-hard re-length))
  (map re->json questions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (re-as-string re)
  (match re
    [(EMPTY) "(EMPTY)"]
    [(LAMBDA) "(LAMBDA)"]
    [(SYMBOL a) (string-append "(SYMBOL #\\" (string a) ")")]
    [(CONCATENATION r s) (string-append "(CONCATENATION " (re-as-string r) " " (re-as-string s) ")")]
    [(KLEENE-CLOSURE r) (string-append "(KLEENE-CLOSURE " (re-as-string r) ")")]
    [(UNION r s) (string-append "(UNION " (re-as-string r) " " (re-as-string s) ")")]
    [(INTERSECTION r s) (string-append "(INTERSECTION " (re-as-string r) " " (re-as-string s) ")")]
    [(COMPLEMENT r) (string-append "(COMPLEMENT " (re-as-string r) ")")]))

(define (re->cell question num)
  (list (hasheq 'cell_type "markdown"
                'metadata (hasheq)
                'source (list (string-append "#### Questão " (number->string num))))
        (hasheq 'cell_type "markdown"
                'metadata (hasheq)
                'source (list (string-append "Crie um AFD equivalente a seguinte expressão regular: " (hash-ref question 'string))))
        (hasheq 'cell_type "code"
                'execution_count 0
                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                'outputs (list)
                'source (list (string-append "(define resposta" (number->string num)) "    null" ")"))
        (hasheq 'cell_type "code"
                'execution_count 0
                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                'outputs (list)
                'source (list (string-append "(automaton-correction resposta"
                                             (number->string num)
                                             " ((compose car dfa-rename-states re-sigma-to-dfa) (list #\\0 #\\1) "
                                             (re-as-string (hash-ref question 're)) "))")))))

(define (generate-cells number-easy number-medium number-hard [re-length MAX-RE-LENGTH])
  (define questions-list (re-list->jsexpr number-easy number-medium number-hard re-length))
  (define require-setup (hasheq 'cell_type "code"
                                'execution_count 0
                                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                                'outputs (list)
                                'source (list "(require \"re.rkt\" \"re-to-dfa.rkt\" \"../dfa/automaton-correction.rkt\" \"../dfa/image-builder.rkt\" \"../dfa/core.rkt\" \"../fa.rkt\")")))
  (flatten (list require-setup (map re->cell
                                    questions-list
                                    (build-list (length questions-list) (lambda (x) (add1 x)))))))

(define (generate-metadata)
  (hasheq 'kernelspec (hasheq 'display_name "Racket"
                              'language "racket"
                              'name "racket")
          'language_info (hasheq 'codemirror_mode "scheme"
                                 'file_extension ".rkt"
                                 'mimetype "text/x-racket"
                                 'name "Racket"
                                 'pygments_lexer "racket"
                                 'version "8.10")))

(define (generate-notebook number-easy number-medium number-hard [re-length MAX-RE-LENGTH])
  (hasheq 'cells (generate-cells number-easy number-medium number-hard re-length)
          'metadata (generate-metadata)
          'nbformat 4
          'nbformat_minor 2))

(define (save-notebook notebook filename)
  (define out (open-output-file filename #:mode 'text #:exists 'truncate))
  (write-json notebook out)
  (close-output-port out))








