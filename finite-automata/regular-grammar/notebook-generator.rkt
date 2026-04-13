#lang racket

(require json
         "../dfa/core.rkt"
         "../dfa/image-builder.rkt"
         "../nfa/image-builder.rkt"
         "grammar.rkt"
         "grammar-to-nfa.rkt"
         "grammar-generator.rkt"
         rackcheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-questions quant min-var max-var [alphabet '(|0| |1|)] [max-rule 15])
  (sample (gen:grammar max-var max-rule alphabet min-var) quant))

(define (grammar-list->jsexpr number-easy number-medium number-hard)
  (define easy   (generate-questions number-easy   1 4))
  (define medium (generate-questions number-medium 5 8))
  (define hard   (generate-questions number-hard   9 12))
  (append easy medium hard))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grammar->cell question num)
  (list (hasheq 'cell_type "markdown"
                'metadata (hasheq)
                'source (list (string-append "#### Questão " (number->string num))))
        (hasheq 'cell_type "markdown"
                'metadata (hasheq)
                'source (list "Crie uma gramática equivalente ao seguinte AFD: "))
        (hasheq 'cell_type "code"
                'execution_count 0
                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                'outputs (list)
                'source (list (format "((compose dfa->pict nfa->dfa grammar->nfa) ~v)" question)))
        (hasheq 'cell_type "code"
                'execution_count 0
                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                'outputs (list)
                'source (list (string-append "(define resposta" (number->string num)) "    null" ")"))
        (hasheq 'cell_type "code"
                'execution_count 0
                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                'outputs (list)
                'source (list
                         (format
                          "(automaton-correction ((compose nfa->dfa grammar->nfa) resposta~a) ((compose nfa->dfa grammar->nfa) ~v))" num question)))))

(define (generate-cells number-easy number-medium number-hard)
  (define questions-list (grammar-list->jsexpr number-easy number-medium number-hard))
  (define require-setup (hasheq 'cell_type "code"
                                'execution_count 0
                                'metadata (hasheq 'vscode (hasheq 'languageId "racket"))
                                'outputs (list)
                                'source (list "#lang iracket/lang #:require racket"
                                              "(require \"grammar.rkt\" \"grammar-to-nfa.rkt\" \"../nfa/subset-construction.rkt\" \"../dfa/automaton-correction.rkt\" \"../dfa/image-builder.rkt\")")))
  (flatten (list require-setup (map grammar->cell
                                    questions-list
                                    (range 1 (add1 (length questions-list)))))))

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

(define (generate-notebook number-easy number-medium number-hard)
  (hasheq 'cells (generate-cells number-easy number-medium number-hard)
          'metadata (generate-metadata)
          'nbformat 4
          'nbformat_minor 2))

(define (save-notebook notebook filename)
  (define out (open-output-file filename #:mode 'text #:exists 'truncate))
  (write-json notebook out)
  (close-output-port out))
