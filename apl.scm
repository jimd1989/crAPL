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
     (λ (⍺ ⍵) (if (negative? ⍺) (⊖ (take (⊖ ⍵) (% ⍺))) (take ⍵ ⍺))))

(← ↓ cdr
     (λ (⍺ ⍵) (if (negative? ⍺) (⊖ (drop (⊖ ⍵) (% ⍺))) (drop ⍵ ⍺))))

(← ⊂ (λ (⍵) `(,⍵))
     cons)

(define (∇⊆g⌂ ⍺ ⍵)
  (cond ((boolean? ⍺) ⍺)
        ((procedure? ⍺) (⍺ ⍵))
        (else (equal? ⍺ ⍵))))

(define (∇⊆g ⍺ ⍵)
  (let ((xs (¨← (λ (acc x)
                 (let ((preds (↑ acc))
                       (parts (↑↓ acc)))
                   (if (∇⊆g⌂ (↑ preds) x)
                     (⊂ (↓ preds) (⊂ (⊂ (⊂ x (↑ parts)) (↓ parts))))
                     (if (⍬? (↑ parts))
                       (⊂ (↓ preds) (⊂ parts))
                       (⊂ (↓ preds) (⊂ (⊂ ⍬ parts)))))))
                (if (list? ⍺)
                  `(,(⌂ circular-list ⍺) (()))
                  `(,(⌂ circular-list (⊂ ⍺)) (())))
                ⍵)))
    (if (⍬? (↑ (↑↓ xs)))
      (⊖ (¨ ⊖ (↓ (↑↓ xs))))
      (⊖ (¨ ⊖ (↑↓ xs))))))

(← ⊆ (λ (⍵) (¨ ⊂ ⍵))
     ∇⊆g)

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
    (¨ f ⍵)))

(← ⌽ (λ (⍵  ) (last-axis (λ (x) (⊖ x)) ⍵))
     (λ (⍺ ⍵) (last-axis (λ (x) (⊖ ⍺ x)) ⍵)))

(define (↑∞ ⍺ ⍵∞)
  `(,(↑ ⍺ ⍵∞) ,(↓ ⍺ ⍵∞)))

(define (∇⍴f ⍵)
  (if (list? (↑ ⍵))
    (⊂ (≢ ⍵) (∇⍴f (↑ ⍵)))
    (⊂ (≢ ⍵))))

(define (∇⍴g ⍺ ⍵)
  (if (= (≢ ⍺) 1)
    (↑∞ (↑ ⍺) ⍵)
    (let ((shape (¨← (λ (acc _)
                      (let ((r (∇⍴g (↓ ⍺) (↑↓ acc))))
                        `(,(⊂ (↑ r) (↑ acc)) ,(↑↓ r))))
                     `(() ,⍵)
                     (⍳ (↑ ⍺)))))
      `(,(⊖ (↑ shape)) ,(↑↓ shape)))))

(← ⍴ ∇⍴f
     (λ (⍺ ⍵) (↑ (∇⍴g ⍺ (⌂ circular-list ⍵)))))

(define (∇⍉f ⍵)
  (cond ((not (list? (↑ ⍵))) ⍵)
        ((⍬? (↑ ⍵)) ⍬)
        (else (⊂ (¨ ↑ ⍵) (∇⍉f (¨ ↓ ⍵))))))

(← ⍉ ∇⍉f)

(← ⌈ (λ (⍵  ) (≅⊥=(ceiling ⍵)))
     (λ (⍺ ⍵) (if (> ⍺ ⍵) ⍺ ⍵)))

(← ⌊ (λ (⍵  ) (≅⊥= (floor ⍵)))
     (λ (⍺ ⍵) (if (< ⍺ ⍵) ⍺ ⍵)))

(← ߸ flatten
     append)

(← ÷ (λ (⍵) (/ 1 ⍵))
     /)

; not very useful in its current form. Undocumented and unsupported for now.
(define (∇⍣g ⍺ ⍵)
  (if (= ⍺ 1)
    (λ (x) (⍵ x))
    (λ (x) (⍵ ((∇⍣g (- ⍺ 1) ⍵) x)))))

(← ⍣ (λ (⍵) (λ (x) (⌂ ⍵ x)))
     ∇⍣g)
