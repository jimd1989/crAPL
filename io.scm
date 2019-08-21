; input/output procedures

(define (⎕← ⍵)
  (if (list? ⍵)
    (begin (for-each ⎕← ⍵) (newline))
    (begin (display ⍵) (display #\space))))
