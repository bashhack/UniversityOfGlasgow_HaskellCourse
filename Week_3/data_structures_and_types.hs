{-|
  ==============================================================================
  3.2 - Recursive Functions on Lists
  ==============================================================================
-}

{-|
  --------------------
  Computing With Lists
  --------------------
-}

-- Two main ways of working with lists:
-- 1) Write functions to do what we want, using recursive definitions that
--    traverse the list data structure
-- 2) Use combinations of standard lib list processing functions

-- We want to use method (2) where possible -- NOTE: the standard lib
-- functions use recursive definitions under the hood

{-|
  ------------------
  Recursion On Lists
  ------------------
-}

-- A list is built from an empty list...
[]
-- ...and the function `cons`, NOTE: We use the (:) operator to represent `cons`
cons :: a -> [a] -> [a]

-- Every list must be either [] or (x:xs) for some x (head) / xs (tail)

-- The recursive definition follows the structure of the data:
-- * Base case of the recursion is []
-- * Recursion (or induction) case is (x:xs)

{- Recursive definition of `length` -}
length :: [a] -> Int
length [] = 0 -- the base case
length (x:xs) = 1 + length xs -- the recursion case

{- Recursive definition of `filter` -}
-- `filter` is given a `predicate (a function that returns a Boolean)`, a list,
-- and returns a list of elements that satisfy the predicate
filter :: (a->Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)
    | pred x = x : filter pred xs
    | otherwise = filter pred xs

{-|
  -----------------------
  Computations Over Lists
  -----------------------
-}

-- Many computations that would be a for/while loop in an imperative
-- language are expressed as list computations in functional ones

-- There are a few common examples:
-- 1) Perform a computation on eachelement of a list: `map`
-- 2) Iterate over a list, from left to right: `foldl`
-- 3) Iterate over a list, from right to left: `foldr`

-- Best practice dictates that we should strive to use these three functions
-- where possible and applicable

{- Function composition -}
-- We can convey complex and/or large computations by method chaining
-- functions which perform smaller computations

-- Composition works like this:
-- 1) Start with an argument of type `a`
-- 2) Apply a function `g :: a -> b` to get it, getting an intermediate result
--    of type `b`
-- 3) Then apply a function `f :: b -> c` to the intermediate result, getting
--    the final result of type `c`

-- We use the (.) notation as the function composition operator
(.) :: (b->c) -> (a->b) -> a -> c
(f . g) x = f (g x)

{-|
  ---------------------------------------------------------
  Performing An Operation On Every Element Of A List: `map`
  ---------------------------------------------------------
-}

-- `map` applies a function to every element of a list
map f [x0,x1,x2] -- > [f x0, f x1, f x2]

{- Composition of maps -}
-- `map` is one of our most commonly used tools in the functional toolkit
-- A common style is to define a set of computations using map and compose them
map f (map g xs) = map (f . g) xs

{- Recursive definition of `map` -}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

{-|
  --------------------------
  Folding A List (reduction)
  --------------------------
-}

-- NOTE: This (in addition to map and filter) has turned out to be not
-- altogether unfamiliar, as I use these all the time in JavaScript when
-- writing in a functional paradigm as the function `reduce`

-- The primary function of folding a list is to iterate over a list to produce
-- a singleton value

{- Left fold: `foldl` -}
-- `foldl` is "fold from the left", or iterating over a list from left to right
-- A typical application is `foldl f z xs`
-- Here, `z :: b` is our initial value
-- The `xs :: [a]` argument is a list of values we combine using the fn `f`
-- We sometimes call the `z :: b` argument an "accumulator"
foldl :: (b->a->b) -> b -> [a] -> b

{- Example of `foldl` with function notation -}
foldl f z [] -- > z
foldl f z [x0] -- > f z x0
foldl f z [x0,x1] -- > f (f z x0) x1
foldl f z [x0,x1,x2] -- > f (f (f z x0) x1) x2

{- Example of `foldl` with infix notation -}
foldl (+) z [] -- > z
foldl (+) z [x0] -- > z + x0
foldl (+) z [x0,x1] -- > (z + x0) + x1
foldl (+) z [x0,x1,x2] -- > ((z + x0) + x1) + x2

{- Recursive definition of `foldl` -}
foldl :: (b->a->b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
    where
        lgo z [] = z
        lgo z (x:xs) = lgo (f z x) xs

{- Right fold: `foldr` -}
foldr :: (a->b->b) -> b -> [a] -> b

{- Example of `foldr` with function notation -}
foldr f z [] -- > z
foldr f z [x0] -- > f x0 z
foldr f z [x0,x1] -- > f x0 (f x1 z)
foldr f z [x0,x1,x2] -- > f x0 (f x1 (f x2 z))

{- Example of `foldr` with infix notation -}
foldr (+) z [] -- > z
foldr (+) z [x0] -- > x0 +  z
foldr (+) z [x0,x1] -- > x0 + (x1 + z)
foldr (+) z [x0,x1,x2] -- > x0 + (x1 + (x2 + z))

{- Recursive definition of `foldr` -}
foldr :: (a->b->b) -> b -> [a] -> b
foldr k z = go
    where
        go [] = z
        go (y:ys) = y `k` go ys

{- Relationship between `foldr` and list structure -}
-- Remember that lists, like [x0,x1,x2] can be written as:
x0 : x1 : x2 : []

-- Folding `cons` over a list, with an empty list as accumulator:
foldr (:) [] [x0,x1,x2] -- > x0 : x1 : x2 : []

{- Some applications of folds -}
sum xs = foldr (+) 0 xs
product xs = foldr (*) 1 xs

-- We can express these same functions by factoring out our lists (xs):
sum = foldr (+) 0
product = foldr (*) 1
-- NOTE: We call this type of notation "point free" style because we are
-- programming solely with the functions, the data isn't mentioned directly


{-|
  ==============================================================================
  3.3 - Functional Maps And Folds Versus Imperative Loops
  ==============================================================================
-}

{- MAP -}
-- Ex. Haskell
lst_ = map f lst

f x = x*(x+1)

lst = [1..10]

main = do
    print lst_

-- > [2,6,12,20,30,42,56,72,90,110]

-- Ex. Ruby
-- def f(x)
--     x*(x+1)
-- end
--
-- lst = []
-- for i in 1 .. 10
--     lst.push(i)
-- end
--
-- lst_ = []
--
-- for elt in lst
--     lst_.push( f(elt) )


{- FOLDL -}
-- Ex. Haskell
main = do
    print lst_
    print accl

g = (/)

accl = foldl g 1 lst

-- Ex. Ruby
-- def g(accl, elt)
--     acc/elt
-- end
--
-- acc = 1
-- for elt in lst
--     acc = g(acc, elt)
-- end
--
-- puts acc

{- FOLDR -}
-- Ex. Haskell
main = do
    print lst_
    print accl
    print accr

g = (/)
g' = (/)
accl = foldl g 1 lst

accr = foldr g' 1 lst

-- Ex. Ruby
-- def g_(acc, elt)
--     acc/elt
-- end
--
-- acc = 1
-- for elt in lst.reverse
--     acc = g_(acc, elt)
-- end
--
-- puts acc


{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|
  -----------------------------------------------------------------------------
  -- NOTE: There are a few ways to work with 'conditional' statements in
  -- Haskell. The easiest way is to write a definition for each case:

  -- Ex. Using length...
  length [] = 0
  length x:xs = 1 + length xs

  However, we may sometimes need a 'single-line expression':

  length lst =
    if lst == []
      then 0
      else let x:xs = lst in 1 + length xs

  Alternatively, we can use 'guards':

  length lst
    | lst == [] = 0
    | otherwise = let x:xs = lst in 1 + length xs

  Finally, we could use the where-clause and semicolons:

  length :: [a] -> Int
  length [] = 0 -- the base case
  length (x:xs) = 1 + length xs -- the recursion case

  ...becomes...

  f = f' where f' 1 = 0; f' x = x + f' (x-1)

  ...in practice...

  length [1..10] -- > 10


  -- Ex. Using filter...
  filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

  ...becomes...

  g = g' where g' p xs = [x | x <- xs, p x]

  ...we could also write...

  filter pred lst
    | null lst = []
    | otherwise = if pred x
      then x:filter pred xs
      else filter pred xs
        where x:xs=lst
  -----------------------------------------------------------------------------

  λ f = f' where f' 1 = 0; f' x = x + f' (x-1)
  -- :: ? -> ?
  λ length = length' where length' 1 = 0; length' x = x + length' (x-1)
  :: ? -> ?
  λ length [1..10]
  10
  -- :: Int
  λ length [1]
  1
  -- :: Int
  λ length [1]
  1
  -- :: Int
  λ filter (<5) [3,9,2,12,6,4]
  [3,2,4]
  -- :: (Num a, Ord a) => [a]
  λ filter = filter' where filter' p xs = [x | x <- xs, p x]
  -- :: ? -> ?
  λ filter (<5) [3,9,2,12,6,4]
  [3,2,4]
  -- :: (Num a, Ord a) => [a]
  λ filter pred lst | null lst = [] | otherwise = if pred x then x:filter pred xs else filter pred xs where x:xs=lst
  -- :: ? -> ?
  λ filter (>5) [3,9,2,12,6,4]
  [9,12,6]
  -- :: (Num a, Ord a) => [a]

  -- NOTE: Start of 3.5, 'Do It Yourself: Function Composition'

  λ map ((*2) . (+3)) [1..10]
  [8,10,12,14,16,18,20,22,24,26]
  -- :: (Enum b, Num b) => [b]
  λ foldl (\acc elt -> elt:acc) "" "Reversing a string"
  "gnirts a gnisreveR"
  -- :: [Char]
  λ foldr (\elt acc -> acc++[elt]) "" "Reversing a string"
  "gnirts a gnisreveR"
  -- :: [Char]
  λ quotient = foldr (/) 2
  -- :: ? -> ?
  λ quotient [1..10]
  0.4921875
  -- :: (Enum b, Fractional b) => b

  -- NOTE: `foldl` has been much easier to grasp for some reason for me,
  -- so I wanted to work more with foldr to see "what's happening under the hood"
  
  foldr (/) 1 [2,4,8]
  -- > 8 / 1 => 8 => 4 / 8 => 0.5 => 0.5 / 2 => 4.0

  foldr (+) 5 [1,2,3,4]
  -- > 5 + 4 + 3 + 2 + 1 = 15

  foldr (/) 2 [3,6,9]
  -- > 9 / 2 => 4.5 => 6 / 4.5 => 1.33333... => 3 / 1.333333 => 2.25

  foldl (/) 16 [8,4,2,1]
  -- > 16 / 8 => 2 => 2 / 4 => 0.5 => 0.5 / 2 => 0.25 => 0.25 / 1 => 0.25
-}
