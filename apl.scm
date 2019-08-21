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

(← ⊂ (λ (⍵) `(,⍵))
     cons)

(define (∇⊆g⌂ ⍺ ⍵)
  (cond ((boolean? ⍺) ⍺)
        ((procedure? ⍺) (⍺ ⍵))
        (else (equal? ⍺ ⍵))))

(define (∇⊆g ⍺ ⍵)
  (let ((xs (↑↓↓
              (→/ (λ (x acc)
                   (let* ((scalar? (eq? (↑ acc) 'scalar))
                          (head (if scalar?
                                  `(scalar ,(↑↓ acc))
                                  `(vector ,(↓ (↑↓ acc)))))
                          (pred (if scalar? (↑↓ acc) (↑ (↑↓ acc))))
                          (tail (↑↓↓ acc)))
                     (if (∇⊆g⌂ pred x)
                       (߸ head (⊂ `(,(⊂ x (↑ tail)) ,@(↓ tail))))
                       (if (⍬? (↑ tail))
                         (߸ head (⊂ tail))
                         (߸ head (⊂ `(() ,@tail)))))))
                  (if (not (list? ⍺))
                    `(scalar ,⍺ (()))
                    `(vector ,(⊖ ⍺) (())))
                  ⍵))))
    (if (⍬? (↑ xs))
      (↓ xs)
      xs)))

(← ⊆ (λ (⍵) (→ ⊂ ⍵))
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
    (→ f ⍵)))

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
    (let ((shape (←/ (λ (acc _)
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
        (else (⊂ (→ ↑ ⍵) (∇⍉f (→ ↓ ⍵))))))

(← ⍉ ∇⍉f)

(← ⌈ (λ (⍵  ) (≅⊥=(ceiling ⍵)))
     (λ (⍺ ⍵) (if (> ⍺ ⍵) ⍺ ⍵)))

(← ⌊ (λ (⍵  ) (≅⊥= (floor ⍵)))
     (λ (⍺ ⍵) (if (< ⍺ ⍵) ⍺ ⍵)))

(← ߸ flatten
     append)
