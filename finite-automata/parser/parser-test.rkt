#lang racket
(require "parser.rkt" brag/support)

(parse (open-input-string "type = dfa"))
;;; (parse-to-datum (apply-tokenizer make-tokenizer "type = dfa"))