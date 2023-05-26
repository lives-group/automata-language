#lang racket

(require "../fa.rkt"
         "../fa-image-builder.rkt"
         "../../utils/dot.rkt"
         "core.rkt"
         racket/system
         racket/contract
         pict)

(provide dfa->dot
         dfa->pict)

;; creating a dot program for the dfa

(define dfa-configs
  (list (def-attr 'top-level 'rankdir "LR")
        (def-attr 'top-level 'size "8.5")
        (def-node 'qi
          (list (def-attr 'node 'shape "point")))))

(define/contract (dfa->dot m)
  (dfa? . -> . dot?)
  (match m
    [(fa ty states sigma delta in final)
     (create-dot 'digraph
                 (append dfa-configs
                         (mk-nodes states final)
                         (mk-edges delta)
                         (mark-initial in)))]))

;; creating a picture for a DFA

(define (dfa->pict m)
  (define code (dot-code (dfa->dot m)))
  (define dot-path (find-executable-path "/run/current-system/sw/bin/dot"))
  (define dot-file (make-temporary-file "~a.dv"))
  (define fig-file (make-temporary-file "~a.png"))
  (begin
    (display-to-file code dot-file #:exists 'truncate)
    (apply system* dot-path (list "-Tpng" dot-file "-o" fig-file))
    (bitmap fig-file)))
