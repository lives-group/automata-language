#lang racket

(require "core.rkt"
         "../dfa/image-builder.rkt"
         "../fa.rkt"
         "image-builder.rkt"
         "table-minimization.rkt")

(provide mk-union-dfa 
         mk-intersection-dfa
         complement)


; generate a list with the initial states of dfa
(define (product-start dfa1 dfa2)
  (first (convert-string-symbol (combination-states
                                 (list (dfa-start dfa1))
                                 (list (dfa-start dfa2))))))

; convert a list of symbols to string
(define (convert-symbol-string list-symbols)
  (map symbol->string list-symbols))

; convert a list of string to symbol
(define (convert-string-symbol list-string)
  (map string->symbol list-string))

; generate the combination of all possible states using strings
(define (combination-states states1 states2)
  (define str-states1 (convert-symbol-string states1))
  (define str-states2 (convert-symbol-string states2))
  (apply append (map (lambda (s1) 
                       (map (lambda (s2)
                              (string-append s1 s2)) str-states2)) str-states1)))

; generate a list with the combination of states from two dfa
(define (product-states dfa1 dfa2)
  (define states-combination 
    (combination-states 
     (dfa-states dfa1) (dfa-states dfa2)))
  (convert-string-symbol states-combination))

; generate a list of all transitions that starts with a state
(define (find-state-transitions state list-delta)
  (filter (lambda (transition)
            (eq? state (car (car transition)))) list-delta))

; generate a list of all transitions that has a espected symbol
(define (find-symbol-transitions symb list-delta)
  (filter (lambda (transition)
            (eq? symb (cdr (car transition)))) list-delta))

; generate a product of n transitions from different fa considering two differnt states
(define (product-transitions list-delta1 list-delta2 state1 state2 symb)
  (define s1 (find-state-transitions state1 list-delta1))
  (define s2 (find-state-transitions state2 list-delta2))
  (define t1 (find-symbol-transitions symb s1))
  (define t2 (find-symbol-transitions symb s2))
  (first (apply append (map (lambda (s1)
                              (map (lambda (s2)
                                     (product-transition s1 s2 symb)) t2)) t1))))

; generate a product of two transitions of a symbol
(define (product-transition transition1 transition2 symb)
  (define origin-state 
    (string->symbol (string-append 
                     (symbol->string (car (car transition1)))
                     (symbol->string (car (car transition2))))))
  (define destiny-state 
    (string->symbol (string-append 
                     (symbol->string (cdr transition1))
                     (symbol->string (cdr transition2)))))
  (cons (cons origin-state symb) destiny-state))


; generate a list with a cartesian product of all transitions of two dfa
(define (dfa-product dfa1 dfa2)
  (apply append (map (lambda (s1)
                       (apply append (map (lambda (s2)
                                            (map (lambda (symb)
                                                   (product-transitions (dfa-delta dfa1) (dfa-delta dfa2) s1 s2 symb))
                                                 (dfa-sigma dfa1)))
                                          (dfa-states dfa2))))
                     (dfa-states dfa1))))

; function that returns if a string is a substring of another string
(define (substring? substring string)
  (regexp-match (regexp substring) string))

; generate a list with all final states considering a intersection of two dfas
(define (dfa-intersection-states dfa1 dfa2)
  (convert-string-symbol (combination-states (dfa-final dfa1)
                                             (dfa-final dfa2))))

; auxiliar function that generate a list with all states considering a state of one dfa
(define (find-union-states list-states final-state)
  (define final (symbol->string final-state))
  (filter (lambda (state)
            (substring? final (symbol->string state)))
          list-states))

; generate a list with all final states considering a union of two dfas
(define (dfa-union-states dfa1 dfa2)
  (define list-states (product-states dfa1 dfa2))
  (define final-states (append (dfa-final dfa1) (dfa-final dfa2)))
  (remove-duplicates (flatten
                      (map (lambda (state)
                             (find-union-states list-states state)) final-states))))

; auxiliar function to indicate if a value is member of a list returning a boolean
(define (member? arg lst)
  (if (member arg lst) #t #f))


; generate the list of transitions that are reachable from start state
(define (reachable-transitions states transitions)
  (filter (lambda (t)
        (member? (car (car t)) states)) transitions))

(define (reachable-states transitions states)
  (define reachable-states-variable  
    (append states (filter symbol?
                    (append 
                      (map (lambda (t)
                        (if (member? (car (car t)) states)
                          (cdr t)
                          #f))
                      transitions)))))
  
  (if (equal? 
        (remove-duplicates reachable-states-variable) 
        (remove-duplicates states))
      (remove-duplicates states)
      (reachable-states transitions 
        (filter symbol? reachable-states-variable))))


; generate a intersection of two dfas
(define (mk-intersection-dfa dfa1 dfa2)
  (define start (product-start dfa1 dfa2))
  (define sigma (dfa-sigma dfa1))
  (define final (dfa-intersection-states dfa1 dfa2))
  (define aux-delta (dfa-product dfa1 dfa2))
  (define states 
    (reachable-states aux-delta (list start)))
  (define delta (reachable-transitions states aux-delta))
  (mk-dfa states sigma delta start final))

; generate a intersection of two dfas
(define (mk-union-dfa dfa1 dfa2)
  (define start (product-start dfa1 dfa2))
  (define sigma (dfa-sigma dfa1))
  (define final (dfa-union-states dfa1 dfa2))
  (define aux-delta (dfa-product dfa1 dfa2))
  (define states 
    (reachable-states aux-delta (list start)))
  (define delta (reachable-transitions states aux-delta))
  (mk-dfa states sigma delta start final))


(define (complement dfa)
  (define new-final (remq* (dfa-final dfa) (dfa-states dfa)))
  (mk-dfa (dfa-states dfa)
          (dfa-sigma dfa)
          (dfa-delta dfa)
          (dfa-start dfa)
          new-final))