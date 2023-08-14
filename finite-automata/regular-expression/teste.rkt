#lang racket

(require rackcheck
         latex-utils/scribble/unmap
         latex-utils/scribble/math
         latex-utils/scribble/utils
         "re.rkt"
         "re-generator.rkt"
         "re-to-dfa.rkt"
         "../dfa/image-builder.rkt")

;ab*(00|10)*
(define RE (CONCATENATION
            a
            (CONCATENATION
             (KLEENE-CLOSURE b)
             (KLEENE-CLOSURE (UNION
                              (CONCATENATION zero zero)
                              (CONCATENATION um1 zero))))))


(define (new-re max-length)
  (first (sample (gen:re max-length) 1)))


(define open "\\{")
(define close "\\}")

(define (re-as-set e)
  (match e
    [(EMPTY) "\\emptyset"]
    [(LAMBDA) "\\lambda"]
    [(SYMBOL a) (string a)]
    [(CONCATENATION r s) (string-append open
                                        (re-as-set r) " \\cdot " (re-as-set s)
                                        close)]
    [(KLEENE-CLOSURE r) (string-append open
                                       (re-as-set r)
                                       close "^*")]
    [(COMPLEMENT r) (string-append "\\neg" open
                                   (re-as-set r)
                                   close)]
    [(UNION r s) (string-append open
                                (re-as-set r) " \\cup " (re-as-set s)
                                close)]
    [(INTERSECTION r s) (string-append open
                                       (re-as-set r) " \\cap " (re-as-set s)
                                       close)]))