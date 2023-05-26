#lang racket/base

(module+ test
  (require rackunit))

(require "./gui/editor.rkt")

(module+ test
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  (editor-main)
  )