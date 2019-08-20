(import srfi-1 (chicken pretty-print))

(define-syntax λ
  (syntax-rules () ((_ . x) (lambda . x))))

(define-syntax ←
  (syntax-rules ()
    ((_ x f g)
     (define (x . ⍵)
       (if (= (length ⍵) 1) (apply f ⍵) (apply g ⍵))))
    ((_ x f)
     (define (x ⍵) (f ⍵)))))

(define ↑↑ caar)
(define ↑↓ cadr)
(define ↑↓↓ caddr)
(define ↑↓↓↓ cadddr)

(define → map)
(define ←/ foldl)
(define →/ foldr)
(define (←// f ⍵) (foldl f (↑ ⍵) (↓ ⍵)))
(define (→// f ⍵) (foldr f (↑ ⍵) (↓ ⍵)))

(← % abs
     modulo)

(← ≢ length)

(← ↑ car
     (λ (⍺ ⍵) (take ⍵ ⍺)))

(← ↓ cdr
     (λ (⍺ ⍵) (drop ⍵ ⍺)))

(← ⍳ iota
     (λ (⍺ ⍵) (list-index (λ (x) (equal? ⍺ x)) ⍵)))

(← ⊖ reverse
     (λ (⍺ ⍵)
       (let* ((l (≢ ⍵))
              (n (if (negative? ⍺) (- l (% (% ⍺) l)) ⍺)))
         (↑ l (↓ n (apply circular-list ⍵))))))

(define (↑∞ ⍺ ⍵∞)
  `(,(↑ ⍺ ⍵∞) ,(↓ ⍺ ⍵∞)))

(define (∇⍴f ⍵)
  (if (list? ⍵)
    (if (list? (↑ ⍵))
      (cons (≢ ⍵) (∇⍴f (↑ ⍵)))
      `(,(≢ ⍵)))
    '(0)))

(define (∇⍴g ⍺ ⍵)
  (if (= (≢ ⍺) 1)
    (↑∞ (↑ ⍺) ⍵)
    (let ((χ (←/ (λ (acc _)
                   (let ((r (∇⍴g (↓ ⍺) (↑↓ acc))))
                     `(,(cons (↑ r) (↑ acc)) ,(↑↓ r))))
                         (list '() ⍵)
                         (⍳ (↑ ⍺)))))
      `(,(⊖ (↑ χ)) ,(↑↓ χ)))))

(← ⍴ ∇⍴f
     (λ (⍺ ⍵) (↑ (∇⍴g ⍺ (apply circular-list ⍵)))))

(define (∇⍉f ⍵)
  (cond ((or (not (list? ⍵)) (not (list? (↑ ⍵)))) ⍵)
        ((null? (↑ ⍵)) '())
        (else (cons (→ ↑ ⍵) (∇⍉f (→ ↓ ⍵))))))

(← ⍉ ∇⍉f)

(← ⌈ (λ (⍵  ) (inexact->exact (ceiling ⍵)))
     (λ (⍺ ⍵) (if (> ⍺ ⍵) ⍺ ⍵)))

(← ⌊ (λ (⍵  ) (inexact->exact (floor ⍵)))
     (λ (⍺ ⍵) (if (< ⍺ ⍵) ⍺ ⍵)))
