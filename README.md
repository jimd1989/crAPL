# Intro

An experimental collection of procedures that mimic select [APL](https://en.wikipedia.org/wiki/APL_%28programming_language%29) concepts, using a similar syntax. My goal is to make working with lists (especially multi-dimensional ones) painless and concise. This is _not_ and will never be an APL implementation; I'm just taking ideas I like and bolting them on to Scheme. Some syntax and most behavior will differ from what's expected in APL.

# Requirements

+ [Chicken Scheme v. 5](https://call-cc.org/)
+ srfi-1
+ An [APL input method](https://www.dyalog.com/apl-font-keyboard.htm) (you may also want to edit this file to include extra symbols like λ, ⌂, etc.)

# Behavior

Much like in APL, crAPL makes use of single-glyph functions that carry both monadic (single argument) and dyadic (two arguments) definitions. The procedures of a monad and dyad with the same symbol can be very different from one another. Scheme's parentheses make the nature of a function application completely unambiguous. APL monads and dyads `f` are of the forms

      f ⍵
    ⍺ f ⍵

respectively. The Scheme applications of this same `f` are unsurprisingly

    (f ⍵)
    (f ⍺ ⍵)

Some APL symbols conflict with Scheme syntax. They are represented as follows in crAPL

    | → %
    . → ·
    , → ߸

crAPL procedures generally assume to be working on some kind of list. They will not check for other data types. These are standard Lisp lists; there is no extra tagging of rank, etc. As such, there are no implicit mapping operations of scalars against vectors, vectors against vectors, or anything else. Use normal Scheme functions like `(map)`.

# Implemented primitive functions

### (↑ ⍵)

Take the head of list ⍵, like `(car)`.

    (↑ '(1 2 3))
    
          1

### (↑ ⍺ ⍵)

Take ⍺ items from the front of list ⍵.

    (↑ 2 '(1 2 3 4 5 6))
    
          (1 2)

### (↓ ⍵)

Take the tail of list ⍵, like `(cdr)`. Note how this differs from the monadic definition of `↓` in APL. Primitive equivalents of `(cadr)`, `(caar)`, and other common combinations have also been defined as `↑↓`, `↑↑`, etc.

    (↓ '(1 2 3))
    
          (2 3)

### (↓ ⍺ ⍵)

Drop ⍺ items from the front of list ⍵.

    (↓ 2 '(1 2 3 4 5 6))
    
          (3 4 5 6)

### (≢ ⍵)

The length of the topmost axis (list) of ⍵.

    (≢ '((1 2 3)(4 5 6)(7 8 9)))
    
          3

### (⊂ ⍵)

Enclose ⍵ in a list.

    (⊂ 'hey)
    
          (hey)

### (⊂ ⍺ ⍵)

Cons ⍺ to list ⍵.

    (⊂ 'hey '(how are you?))
    
          (hey how are you?)

### (⊆ ⍵)

Enclose all items of ⍵ into a sub-list.

    (⊆ '(hey how are you))
    
          ((hey) (how) (are) (you))

### (⊆ ⍺ ⍵)

Split the items of ⍵ into sublists in terms of ⍺, where ⍺ is a scalar/list of boolean(s), predicate(s), or data.

+ If ⍺ is a boolean, it will accept/reject an index of ⍵ based upon its truthfulness.
+ If ⍺ is a predicate `f`, it will accept/reject an index `x` of ⍵ if `(f x)` evaluates to true.
+ If ⍺ is any other data type, it will accept/reject an index of ⍵ based upon whether or not they are `(equal?)`.

⍺ does not have to be the same length as ⍵. Its indices are mapped across ⍵ in a cycle.

    (⊆ number? '(1 hey 2 3 how 4 are you 5 6 ?))
    
          ((1) (2 3) (4) (5 6))
    
    (⊆ '(#t #f #f #t #t) '(1 2 3 4 5))
    
          ((1) (4 5))
    
    (⊆ '(#t #t #f) '(1 2 3 4 5 6 7))
    
          ((1 2) (4 5) (7))
    
    (⊆ (string->list "davy jones") (string->list "wavy bones"))
    
          ((#\a #\v #\y #\space) (#\o #\n #\e #\s))    

### (% ⍵)

The absolute value of ⍵.

    (% -1)
    
          1


### (% ⍺ ⍵)

⍺ modulo ⍵.

    (% 8 3)
    
          2

### (⍳ ⍵)

A list of integers ⍵ items long, counting up from 0. This differs from APL, which begins its count at 1.

    (⍳ 9)
    
          (0 1 2 3 4 5 6 7 8)

### (⍳ ⍺ ⍵)

The first index of ⍵ that matches ⍺.

    (⍳ 'b '(a b c d e f g))
    
          1

### (⊖ ⍵)

Reverse the topmost axis (list) of ⍵.

    (⊖ '((1 2 3)(4 5 6)(7 8 9)))
    
          ((7 8 9)
           (4 5 6)
           (1 2 3))

### (⊖ ⍺ ⍵)

Rotate the topmost axis of ⍵ by ⍺ items. ⍺ can be a negative integer to rotate in the opposite direction.

    (⊖ -1 (⍳ 9))
    
          (8 0 1 2 3 4 5 6 7)

### (⌽ ⍵)

Reverse the last axis of ⍵.

    (⌽ (⍴ '(3 3) (⍳ 9)))
    
          ((2 1 0)
           (5 4 3)
           (8 7 6))

### (⌽ ⍺ ⍵)

Rotate the last axis of ⍵ by ⍺ items. ⍺ can be a negative integer here too.

    (⌽ 1 (⍴ '(3 3) (⍳ 9)))
    
          ((1 2 0)
           (4 5 3)
           (7 8 6))

### (⍴ ⍵)

The shape of an arbitrarily deep nested list. Dimensions are measured according to the length of the first item at any level in the list. If a nested list is not balanced, this will return inaccurate measurements.

    (⍴ '((1 2 3 3)(4 5 6 6)(7 8 9 9)))
    
          (3 4)

### (⍴ ⍺ ⍵)

Reshape a flat list ⍵ by the dimensions specified in list ⍺. If ⍵ is not long enough to fill these dimensions, it will repeat.

    (⍴ '(3 3 3) (⍳ 4))
    
          (((0 1 2) (3 0 1) (2 3 0))
           ((1 2 3) (0 1 2) (3 0 1))
           ((2 3 0) (1 2 3) (0 1 2)))

### (⍉ ⍵)

Transpose a matrix, turning its rows into columns.

    (⍉ (⍴ '(4 4) (⍳ 16)))
    
          ((0 4 8  12)
           (1 5 9  13)
           (2 6 10 14)
           (3 7 11 15))

### (⌈ ⍵)

The ceiling of a decimal value.

    (⌈ 3.14)
    
          4

### (⌈ ⍺ ⍵)

Select the greater value between ⍺ and ⍵.

    (⌈ 1 2)
    
          2

### (⌊ ⍵)

The floor of a decimal value.

    (⌊ 3.14)
    
          3

### (⌊ ⍺ ⍵)

Select the lesser value between ⍺ and ⍵.

    (⌊ 1 2)
    
          1

### (≡ ⍵)

The maximum depth of ⍵.

    (≡ '(1 (2) ((3)) (((4)))))
    
          4

### (≡ ⍺ ⍵)

Whether or not ⍺ and ⍵ are `(equal?)`.

    (≡ (⍳ 9) (⍳ 9)
    
          #t

### (߸ ⍵)

Flatten a list.

    (߸ '((1 2 (3))(4 5 (6))(7 8 (9))))
    
          (1 2 3 4 5 6 7 8 9)

### (߸ ⍺ ⍵)

Append two lists. Technically not a dyadic function; you can concatenate any number of items since this is just an alias for `(append)`.

    (߸ '(1 2 3) '(4 5 6))
    
          (1 2 3 4 5 6)

### (≠ ⍺ ⍵)

Whether or not ⍺ and ⍵ are not `(equal?)`.

    (≠ 'goose 'moose)
    
          #t

# Other symbols

### (λ (x ...) y ...)

Scheme's normal lambda form can be represented with the Greek symbol, to make it more concise. This is nothing more than syntax sugar.

### (¨ f xs)

Sugar for `(map f xs)`.

### (¨← f acc xs)

Sugar for `(foldl f acc xs)`. The procedure `(¨←← f xs)` performs a left fold where the initial accumulator value is the first item in list `xs.`

### (¨→ f acc xs)

Sugar for `(foldr f acc xs)`. The procedure `(¨→→ f xs)` performs a right fold where the initial accumulator value is the first item in list `xs.`

### (⌂ f . xs)

Sugar for `(apply f xs)`.

### ⍬

Sugar for the null list `'()`. `⍬?` is an alias for `(null?)`.

# Misc examples

Max value of every row in a matrix

    (¨ (λ (⍵) (¨←← ⌈ ⍵)) (⍴ '(3 3) (⍳ 9)))
    
          (2 5 8)

# Caveats

Way too many to list. I should reiterate that this is not an APL implementation. I'm hoping to implement more primitives as time allows.
