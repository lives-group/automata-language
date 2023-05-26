#lang racket

(require "fa.rkt"
         "../utils/dot.rkt"
         racket/system
         racket/contract
         pict)

(provide mk-nodes
         mk-edges
         mark-initial)


;; generic functions for building images
;; from finite automata


(define/contract (mk-nodes states final)
  (list? list? . -> . any/c)
  (define (build-shape s)
    (if (member s final) "doublecircle" "circle"))
  (map (lambda (e)
         (def-node e
           (list (def-attr
                   'node
                   'shape
                   (build-shape e)))))
       states))


(define/contract (mk-edges delta)
  (list? . -> . any/c)
  (map (lambda (e) (def-edge
                     'directed
                     (car (car e))
                     (cdr e)
                     (list (def-attr 'edge 'label (cdr (car e))))))
       delta))

(define/contract (mark-initial in)
  (symbol? . -> . list?)
  (list (def-edge 'directed 'qi in '())))
