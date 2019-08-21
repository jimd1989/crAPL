; APL syntax transformers and implementations of their functionality, where
; need be.

; some APL syntax conflicts with Lisp syntax. The following operators are
; redefined:
;
; | → %
; . → ·
; , → ߸

(import srfi-1 (chicken pretty-print))
(load "list-procedures.scm")
(load "conversions.scm")
(load "io.scm")

(define-syntax λ
  (syntax-rules () ((_ . x) (lambda . x))))

(define-syntax ←
  (syntax-rules ()
    ((_ x f  ) (define (x ⍵) (f ⍵)))
    ((_ x f g) (define (x . ⍵) (if (= (≢ ⍵) 1) (⌂ f ⍵) (⌂ g ⍵))))))

(← % abs
     modulo)

(← ≢ length)

(← ↑ car
     (λ (⍺ ⍵) (take ⍵ ⍺)))

(← ↓ cdr
     (λ (⍺ ⍵) (drop ⍵ ⍺)))

(← ⍳ iota
     (λ (⍺ ⍵) (list-index (λ (x) (equal? ⍺ x)) ⍵)))

(define (∇⊖g ⍺ ⍵)
  (let* ((len (≢ ⍵))
         (rot (if (negative? ⍺) (- len (% (% ⍺) len)) ⍺)))
    (↑ len (↓ rot (⌂ circular-list ⍵)))))

(← ⊖ reverse
     ∇⊖g)

(define (last-axis f ⍵)
  (if (not (list? ⍵))
    (f ⍵)
    (→ f ⍵)))

(← ⌽ (λ (⍵  ) (last-axis (λ (x) (⊖ x)) ⍵))
     (λ (⍺ ⍵) (last-axis (λ (x) (⊖ ⍺ x)) ⍵)))

(define (↑∞ ⍺ ⍵∞)
  `(,(↑ ⍺ ⍵∞) ,(↓ ⍺ ⍵∞)))

(define (∇⍴f ⍵)
  (if (list? (↑ ⍵))
    (cons (≢ ⍵) (∇⍴f (↑ ⍵)))
    `(,(≢ ⍵))))

(define (∇⍴g ⍺ ⍵)
  (if (= (≢ ⍺) 1)
    (↑∞ (↑ ⍺) ⍵)
    (let ((shape (←/ (λ (acc _)
                      (let ((r (∇⍴g (↓ ⍺) (↑↓ acc))))
                        `(,(cons (↑ r) (↑ acc)) ,(↑↓ r))))
                     `(() ,⍵)
                     (⍳ (↑ ⍺)))))
      `(,(⊖ (↑ shape)) ,(↑↓ shape)))))

(← ⍴ ∇⍴f
     (λ (⍺ ⍵) (↑ (∇⍴g ⍺ (⌂ circular-list ⍵)))))

(define (∇⍉f ⍵)
  (cond ((not (list? (↑ ⍵))) ⍵)
        ((⍬? (↑ ⍵)) ⍬)
        (else (cons (→ ↑ ⍵) (∇⍉f (→ ↓ ⍵))))))

(← ⍉ ∇⍉f)

(← ⌈ (λ (⍵  ) (≅⊥=(ceiling ⍵)))
     (λ (⍺ ⍵) (if (> ⍺ ⍵) ⍺ ⍵)))

(← ⌊ (λ (⍵  ) (≅⊥= (floor ⍵)))
     (λ (⍺ ⍵) (if (< ⍺ ⍵) ⍺ ⍵)))

(← ߸ flatten
     append)
