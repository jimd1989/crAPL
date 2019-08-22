; terse redefinitions of common list-altering procedures

(import srfi-1)

(define ↑↑ caar)
(define ↑↑↑ caaar)
(define ↑↑↑↑ caaaar)
(define ↑↓ cadr)
(define ↑↓↓ caddr)
(define ↑↓↓↓ cadddr)
(define ↓↓ cddr)
(define ↓↓↓ cdddr)
(define ↓↓↓↓ cddddr)

(define → map)
(define →/ foldr)
(define (→// f ⍵) (foldr f (↑ ⍵) (↓ ⍵)))
(define ←/ foldl)
(define (←// f ⍵) (foldl f (↑ ⍵) (↓ ⍵)))
(define ⌂ apply)
(define ⍬ '())
(define ⍬? null?)
