#lang racket

(require syntax/parse
         (except-in pprint char)
         (for-syntax syntax/parse
                     racket))

(provide create-dot
         def-attr
         def-node
         def-edge
         dot
         node
         edge
         attr
         dot-code
         dot?)


#|
DOT: simple representation of graphviz
dot programs
|#


(struct dot-spec
  (type stmts)
  #:transparent)

(struct node-stmt
  (node-id attrs)
  #:transparent)

(struct edge-stmt
  (type origin targed attrs)
  #:transparent)

(struct attr-stmt
  (type left-id right-id)
  #:transparent)

;; creating a dot file

(define (create-dot type stmts)
  (dot-spec type stmts))

(define (def-attr type left right)
  (attr-stmt type left right))

(define (def-node node-id attrs)
  (node-stmt node-id attrs))

(define (def-edge type origin target attrs)
  (edge-stmt type origin target attrs))

(define (dot? e)
  (dot-spec? e))

;; macros for building the dot file spec

(define-syntax (attr stx)
  (syntax-parse stx #:datum-literals (=)
    [(attr left:id = right:expr)
      #'(attr-stmt 'top-level 'left 'right)]))


(define-syntax (edge stx)
  (syntax-parse stx #:datum-literals (= -> --)
    [(edge source -> target:)
     #'(edge-stmt 'directed 'source 'target '())]
    [(edge source -> target (attr:id = val:expr) ...)
     #'(edge-stmt 'directed 'source 'target (list (attr-stmt 'edge 'attr 'val) ...))]
    [(edge source -- target)
     #'(edge-stmt 'undirected 'source 'target '())]
    [(edge source -- target (attr:id = val:expr) ...)
     #'(edge-stmt 'undirected 'source 'target (list (attr-stmt 'edge 'attr 'val) ...))]))

(define-syntax (node stx)
  (syntax-parse stx #:datum-literals (=)
    [(node name:id) #'(node-stmt 'name '())]
    [(node name:id (attr:id = val:expr) ...)
       #'(node-stmt 'name (list (attr-stmt 'node 'attr 'val) ...))]))

(define-syntax (dot stx)
  (syntax-parse stx #:datum-literals (graph digraph)
    [(dot graph stmt ...)
     #'(dot-spec 'graph (list stmt ...))]
    [(dot digraph stmt ...)
     #'(dot-spec 'digraph (list stmt ...))]))


;; function for pretty-printing dot code

(define (symbol/p s)
  (text (symbol->string s)))

(define (header type)
  (hs-append (symbol/p type)
             (symbol/p (gensym))))

(define (braces d)
  (v-append lbrace
            (nest 4 d)
            rbrace))

(define (brackets d)
  (hs-append lbracket d rbracket))

(define (attr/p a)
  (match a
    [(attr-stmt type left right)
     (hs-append (symbol/p left)
                equals
                (text (format "~a" right))
                semi)]))

(define (attrs/p as)
  (if (null? as)
      empty
      (hs-append
        (brackets
         (hs-concat
          (apply-infix semi
                       (map attr/p as))))
        semi)))

(define (node/p n)
  (match n
    [(node-stmt nid attrs)
     (hs-append (text "node")
                (attrs/p attrs)
                (symbol/p nid)
                semi)]))

(define (edge/p e)
  (match e
    [(edge-stmt type source target attrs)
     (let ([arr (if (eq? type 'directed)
                    (text "->")
                    (text "--"))])
       (hs-append (symbol/p source)
                  arr
                  (symbol/p target)
                  (attrs/p attrs)))]))

(define (stmt/p s)
  (cond
    [(node-stmt? s) (node/p s)]
    [(edge-stmt? s) (edge/p s)]
    [(attr-stmt? s) (attr/p s)]))


(define (dot/p d)
  (match d
    [(dot-spec type stmts)
     (hs-append
      (header type)
      (braces
       (v-concat
        (map stmt/p stmts))))]))


(define (dot-code g)
  (pretty-format (dot/p g)))


;; simple test

(define S
  (dot digraph
       (attr rankdir = "LR")
       (attr size = "8.5")
       (node S (shape = "doublecircle"))
       (node qi (shape = "point"))
       (node q1 (shape = "circle"))
       (node q2 (shape = "circle"))
       (edge qi -> S)
       (edge S -> q1 (label = "a"))))
