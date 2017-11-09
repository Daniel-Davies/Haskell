Folds
=====

This part of the assignment is concerned with defining folds.

You will need to create a file called `Fold.hs` where you will place
your answers.

In lectures we covered the `foldr` function:

< foldr :: (a -> b -> b) -> b -> [a] -> b
< foldr f y []     = y
< foldr f y (x:xs) = f x (foldr f y xs)

It's worth cementing this type and definition in your mind: it's a
very useful function! Try to understand how it works, and make
sure you can come up with this definition from scratch by yourself.

The idea behind a `foldr` is relatively simple: it's a function
that replaces the *structure* of a list with an *evaluation* of
the list. For instance, let's consider a list of numbers,
and how it might be converted into the sum of its elements.
First, remember that we can write the list `[1,2,3,4,5]` as
`1 : 2 : 3 : 4 : 5 : []`. Let's call this list `xs` and see how we can
manipulate it:

    xs     = 1 : 2 : 3 : 4 : 5 : []
               ↓   ↓   ↓   ↓   ↓  ↓
    sum xs = 1 + 2 + 3 + 4 + 5 +  0 =  15

Notice that summing this list involves replacing the symbol `:` with
`+`, and `[]` with `0`. The actual elements of the list remain
untouched. In other words, the essence of a `sum` operation
is the `+` and the `0`. This observation is captured in the following
definition of `sum`:

< sum :: [Integer] -> Integer
< sum = foldr (+) 0

Now let's try a different operation, the factorial function:

    xs     = 1 : 2 : 3 : 4 : 5 : []
               ↓   ↓   ↓   ↓   ↓  ↓
    fac xs = 1 * 2 * 3 * 4 * 5 *  1 =  120

Again, we leave the elements untouched: we simply replace the
structure of the list with an evaluation. In this case,
we use `*` and `1`. So this gives us the following definition
of `fac`:


> fac :: Integer -> Integer
> fac n = foldr (*) 1 [1 .. n]

So how does the `foldr` function work?
Well, there are two cases for `foldr`: when the list is empty, and
when it is not. When the list is empty, we have:

< foldr f y [] = y

This is the code that replaces the empty list with the value `y`
that is passed in.

To replace the `:` operation, we look to the second case:

< foldr f y (x:xs) = f x (foldr f y xs)

This says that we use `f` and apply it to the incoming `x` as well
as the result of folding `xs`.

Now it's your turn to write some code that folds results.

To make things easier, you should work in a file called `Fold.hs`. You
should modify only that file for the exercises of this assignment.
You should put the following code at the top of the file:

    module Fold where
    import Prelude hiding (sum, product, length, and, or, all, any, filter)

This will hide the prelude definitions of these functions, since the
goal is for you to define them yourself.
Since it's an `hs` file, you should remove any `>` or `<` at the
beginning of code lines.

The following TODOs should all be implemented using `foldr`.
You should use `ghci` to check your implementations are well typed
and that they behave how you expect.

    $ ghci Fold.hs

You should be using ghci extensively to test your work: type in
values to test your functions yourself.

*****************************************************************************
**TODO #F1:** Define a function called `sum :: [Integer] -> Integer`
that sums a list of numbers.
For instance

    sum [1, 2, 3, 4] = 10

*****************************************************************************

*****************************************************************************
**TODO #F2:** Define a function called 
`product :: [Integer] -> Integer` that finds the product of a list of
numbers. For instance

    product [1, 2, 3, 4] = 24

*****************************************************************************

*****************************************************************************
**TODO #F3:** Define a function called `and :: [Bool] -> Bool` that
returns `True` when all the elements of the input list are `True` and
returns `False` otherwise.
For instance

    and [True, False, True, True] = False
    and [True, True, True, True] = True

*****************************************************************************

*****************************************************************************
**TODO #F4:** Define a function called `or :: [Bool] -> Bool` that
returns `True` if at least one element in the input list is `True`
and `False` otherwise.
For instance

    or [False, False, True, False] = True
    or [False, False, False] = False

*****************************************************************************

*****************************************************************************
**TODO #F5:** Define a function called 
`all :: (a -> Bool) -> [a] -> Bool` where the result of
`all p xs` is `True` if for all elements `x` in `xs`, `p x` is `True`.
Otherwise, this should return `False`.
For instance

    all even [0,2,4,6] = True
    all even [0,2,4,5,6] = False

and

    all null [[], [], []] = True

The functions `even` and `null` are already defined for you in the
`Prelude`.
*****************************************************************************

*****************************************************************************
**TODO #F6:** Define a function called 
`any :: (a -> Bool) -> [a] -> Bool` where the result of
`any p xs` is `True` if there exists an element `x` in `xs` where
`p x` is `True`. 
Otherwise, this should return `False`.
For instance

    any even [1,3,5,7] = False
    any even [1,3,4,5,7] = True

*****************************************************************************

*****************************************************************************
**TODO #F7:** Define a function called
`length :: [a] -> Int` that returns the length of a list.
For instance

    length ['a' .. 'z'] = 26

*****************************************************************************

*****************************************************************************
**TODO #F8:** Define a function called
`filter :: (a -> Bool) -> [a] -> [a]` where the result of `filter p
xs` is a list whose elements are those in `xs` for which `p x` holds.
For instance

    filter even [1,2,3,4,5,6] = [2,4,6]

*****************************************************************************

*****************************************************************************
**TODO #F9:** Define a function called
`group :: Eq a => [a] -> [[a]]` that groups elements that are repeated
in the original list. For instance

    group [1,2,2,3,4,4,4,5] = [[1], [2,2], [3], [4,4,4], [5]]
    group [1,2,2,3,4,4,4,5,1,1,1] = [[1], [2,2], [3], [4,4,4], [5], [1,1,1]]

*****************************************************************************

*****************************************************************************
**TODO #F10:** Define a function called `transpose :: [[a]] -> [[a]]`
that takes a square list of lists and turns rows to columns.
For instance

    transpose [[1, 2, 3]
              ,[4, 5, 6]
              ,[7, 8, 9]]
    = 
              [[1, 4, 7]
              ,[2, 5, 8]
              ,[3, 6, 9]]

*****************************************************************************

*****************************************************************************
**TODO #F11:** Define a function called `permute :: [a] -> [[a]]`
that permutes all the elements in a list. For instance

    permute [1 .. 3] = [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

*****************************************************************************

