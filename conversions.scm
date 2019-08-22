; shorthand procedures for converting data types

(define =⊥≅ exact->inexact)
(define ≅⊥= inexact->exact)
(define c⊥n char->integer)
(define n⊥c integer->char)
(define (n⊥b n) (if (= n 0) #f #t))
(define (b⊥n b) (if b 1 0))
(define s⊥x string->symbol)
(define x⊥s symbol->string)
(define s⊥cs string->list)
(define s⊥n string->number)
(define (s⊥ns s) (→ c⊥n (s⊥cs s)))
(define (cs⊥s cs) (⌂ string cs))
