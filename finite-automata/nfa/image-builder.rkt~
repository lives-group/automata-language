#lang racket

(require "../fa.rkt"
         "../fa-image-builder.rkt"
         "../../utils/dot.rkt"
         "core.rkt"
         racket/system
         pict)

(provide nfa->dot
         nfa->pict)

;; creating a dot program for the nfa

(define nfa-configs
  (list (def-attr 'top-level 'rankdir "LR")
        (def-attr 'top-level 'size "8.5")))

;; building the initial states for the NFA

(define (mk-initials in)
  (define point-names
    (map (lambda (n) (string->symbol (format "q~a" n))) in))
  (define (gen-point n)
    (def-node n
              (list (def-attr 'node 'shape "point"))))
  (define points
    (map gen-point point-names))
  (append points
         (map (lambda (n s)
         (def-edge 'directed n s '())) point-names in)))


(define (unfold-edges edges)
  (define (unfold-edge e)
    (match e
      [(cons s xs)
       (map (lambda (x) (cons s x)) (flatten xs))]))
  (append-map unfold-edge edges))

(define (nfa->dot m)
  (match m
    [(fa ty states sigma delta in final)
     (create-dot 'digraph
                 (append nfa-configs
                         (mk-nodes states final)
                         (mk-edges (unfold-edges delta))
                         (mk-initials in)))]))

;; creating a picture for a NFA

(define (nfa->pict m)
  (define code (dot-code (nfa->dot m)))
  (define dot-path (find-executable-path "/usr/local/bin/dot"))
  (define dot-file (make-temporary-file "~a.dv"))
  (define fig-file (make-temporary-file "~a.png"))
  (begin
    (display-to-file code dot-file #:exists 'truncate)
    (apply system* dot-path (list "-Tpng" dot-file "-o" fig-file))
    (bitmap fig-file)))


