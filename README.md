# Intro

An experimental collection of procedures that mimic select [APL](https://en.wikipedia.org/wiki/APL_%28programming_language%29) concepts, using a similar syntax. This is _not_ and will never be an APL implementation; I'm just taking ideas that I like and bolting them on to Scheme. Some syntax and behavior will differ from what's expected in APL. These functions replace the notions of vectors/matrices/tensors with nested Lisp lists. They're convenience functions that are meant to play nicely with Scheme's normal maps, folds, and filters.

# Requirements

+ [Chicken Scheme v. 5](https://call-cc.org/)
+ srfi-1

# Implemented primitives

## (← x f . g)

APL functions are either monadic (one argument) or dyadic (two). The ← form defines function `x` with two procedures `f` and `g`. `f` is applied to `x`'s argument in monadic cases, and `g` to `x`'s arguments in dyadic cases. If `g` is omitted, `x` is strictly a monadic function.

If the expected form of a monadic APL function `f` is

    f ⍵

Then its Scheme application is

    (f ⍵)

Likewise the dyad `g` of the form

    ⍺ g ⍵

Is invoked as

    (g ⍺ ⍵)

This is similar to the way comparison operators work in Lisp. 

## (↑ ⍵)

Take the head of list ⍵, like `(car)`.

    (↑ '(1 2 3))
    
          1

## (↑ ⍺ ⍵)

Take ⍺ items from the front of list ⍵.

    (↑ 2 '(1 2 3 4 5 6))
    
          (1 2)

## (↓ ⍵)

Take the tail of list ⍵, like `(cdr)`. Note how this differs from the monadic definition of `↓` in APL. Primitive equivalents of `(cadr)`, `(caar)`, and other common combinations have also been defined as `↑↓`, `↑↑`, etc.

    (↓ '(1 2 3))
    
          (2 3)

## (↓ ⍺ ⍵)

Drop ⍺ items from the front of list ⍵.

    (↓ 2 '(1 2 3 4 5 6))
    
          (3 4 5 6)

## (≢ ⍵)

The length of the topmost axis (list) of ⍵.

    (≢ '((1 2 3)(4 5 6)(7 8 9)))
    
          3


## (% ⍵)

The absolute value of ⍵.

    (% -1)
    
          1


## (% ⍺ ⍵)

⍺ modulo ⍵.

    (% 8 3)
    
          2

## (⍳ ⍵)

A list of integers ⍵ items long, counting up from 0. This differs from APL, which begins its count at 1.

    (⍳ 9)
    
          (0 1 2 3 4 5 6 7 8)

## (⍳ ⍺ ⍵)

The first index of ⍵ that matches ⍺.

    (⍳ 'b '(a b c d e f g))
    
          1

## (⊖ ⍵)

Reverse the topmost axis (list) of ⍵.

    (⊖ '((1 2 3)(4 5 6)(7 8 9)))
    
          ((7 8 9)
           (4 5 6)
           (1 2 3))

## (⊖ ⍺ ⍵)

Rotate the topmost axis of ⍵ by ⍺ items. ⍺ can be a negative integer to rotate in the opposite direction.

    (⊖ -1 (⍳ 9))
    
          (8 0 1 2 3 4 5 6 7)

## (⍴ ⍵)

The shape of an arbitrarily deep nested list. Dimensions are measured according to the length of the first item at any level in the list. If a nested list is not balanced, this will return inaccurate measurements.

    (⍴ '((1 2 3 3)(4 5 6 6)(7 8 9 9)))
    
          (3 4)

## (⍴ ⍺ ⍵)

Reshape a flat list ⍵ by the dimensions specified in list ⍺. If ⍵ is not long enough to fill these dimensions, it will repeat.

    (⍴ '(3 3 3) (⍳ 4))
    
          (((0 1 2) (3 0 1) (2 3 0))
           ((1 2 3) (0 1 2) (3 0 1))
           ((2 3 0) (1 2 3) (0 1 2)))

## (⌈ ⍵)

The ceiling of a decimal value.

    (⌈ 3.14)
    
          4

## (⌈ ⍺ ⍵)

Select the greater value between ⍺ and ⍵.

    (⌈ 1 2)
    
          2

## (⌊ ⍵)

The floor of a decimal value.

    (⌊ 3.14)
    
          3

## (⌊ ⍺ ⍵)

Select the lesser value between ⍺ and ⍵.

    (⌊ 1 2)
    
          1

# Other symbols

## (λ (x ...) y ...)

Scheme's normal lambda form can be represented with the Greek symbol, to make it more concise. This is nothing more than syntax sugar.

## (→ f xs)

Sugar for `(map f xs)`.

## (←\ f acc xs)

Sugar for `(foldl f acc xs)`. The procedure `(←\\ f xs)` performs a left fold where the initial accumulator value is the first item in list `xs.`

## (→\ f acc xs)

Sugar for `(foldr f acc xs)`. The procedure `(→\\ f xs)` performs a right fold where the initial accumulator value is the first item in list `xs.`

# Caveats

Way too many to list. I should reiterate that this is not an APL implementation. I'm hoping to implement more primitives as time allows.
