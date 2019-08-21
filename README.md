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

# Implemented primitives

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

### (߸ ⍵)

Flatten a list.

    (߸ '((1 2 (3))(4 5 (6))(7 8 (9))))
    
          (1 2 3 4 5 6 7 8 9)

### (߸ ⍺ ⍵)

Append two lists. Technically not a dyadic function; you can concatenate any number of items since this is just an alias for `(append)`.

    (߸ '(1 2 3) '(4 5 6))
    
          (1 2 3 4 5 6)

# Other symbols

### (λ (x ...) y ...)

Scheme's normal lambda form can be represented with the Greek symbol, to make it more concise. This is nothing more than syntax sugar.

### (→ f xs)

Sugar for `(map f xs)`.

### (←\ f acc xs)

Sugar for `(foldl f acc xs)`. The procedure `(←\\ f xs)` performs a left fold where the initial accumulator value is the first item in list `xs.`

### (→\ f acc xs)

Sugar for `(foldr f acc xs)`. The procedure `(→\\ f xs)` performs a right fold where the initial accumulator value is the first item in list `xs.`

### (⌂ f . xs)

Sugar for `(apply f xs)`.

### ⍬

Sugar for the null list `'()`. `⍬?` is an alias for `null?`.

# Misc examples

Max value of every row in a matrix

    (→ (λ (⍵) (←// ⌈ ⍵)) (⍴ '(3 3) (⍳ 9)))
    
          (2 5 8)

# Caveats

Way too many to list. I should reiterate that this is not an APL implementation. I'm hoping to implement more primitives as time allows.
