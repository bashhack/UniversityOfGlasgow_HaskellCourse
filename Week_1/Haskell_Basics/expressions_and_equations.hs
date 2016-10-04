{-|
  ==============================================================================
  1.2 - Basic Elements By Example
  ==============================================================================
-}


{-|
  -----------
  Expressions
  -----------
-}

-- NOTE: In Haskell there are ONLY expressions, there are NO statements

-- Ex. An expression
(b*b-4*a*c)/2*a

-- Ex. Assigning an expression to a variable
v = (b*b-4*a*c)/2*a


{-|
  -----------
  Functions
  -----------
-}

-- Ex. A function in Python, for comparison
-- def hello(name):
--   return "Hello, " + name

-- Ex. Function directly above, in Haskell
hello name = "Hello, " ++ name


{-|
  -----
  Types
  -----
-}

-- Ex. An example of types in C
-- int f (int x, int y) {
--   return x*x+y+y;
-- }

-- Ex. Using Haskell
f :: Int -> Int -> Int
f x y = x*y+x+y


{-|
  -----
  Lists
  -----
-}

-- Ex. A Python, Ruby, or JavaScript list -- but, also a valid Haskell list!
lst = ["A", "list", "of", "strings"]

-- Ex. Joining lists in Python
-- lst = [1,2] + [3,4]
-- Ex. Joining lists in JavaScript
-- lst = [1,2].concat([3,4])

-- Ex. Joining lists in Haskell
joinlist = [1,2] ++ [3,4]


{-|
  -------------------
  Anonymous Functions
  -------------------
-}

-- Ex. An example of an anonymous function in JavaScript
-- var f = (x, y) => x*y+x+y;

-- Ex. Anonymous functions, known as 'lambda' functions, are the core of Haskell
f = \x y -> x*y+x+y


{-|
  ----------------------
  Higher Order Functions
  ----------------------

  -- NOTE: Higher order functions are functions that operate on other functions,
     and are said to either take a function as an arg or return another function
-}

-- Ex. An implementation of 'map' in JavaScript
-- var doubleAddOne = [...Array(10).keys()].map(x => ++x).map(x => x*2+1);

-- Ex. An example of map in Haskell
doubleAddOne = map (\x -> x*2+1) [1..10]


{-|
  ==============================================================================
  1.3 - Introduction to Expressions and Equations
  ==============================================================================
-}


{-|
  -----------
  Expressions
  -----------

  - Again, Haskell has NO statements, ONLY expressions!
  - NOTE TO SELF: This explanation of the samantic and logical differences was
    very helpful, `fsharpforfunandprofit.com/posts/expressions-vs-statements`
  - Haskell is not alone in this, as all pure functional programming languages
    don't have any statements - no assignments and no jumps
  - By virtue of this, ALL computation is done by evaluation of expressions
  - NOTE TO SELF: Here are the docs to the Haskell 98 Report on expressions,
    `www.haskell.org/onlinereport/exps.html`
-}


{-
  The notation shown below, `-- >`, means 'an expression evaluates to
  a result' and is commonly written as `e ⇝ r`
-}

-- Ex. Integer Expressions
2 -- > 2
3+4 -- > 7
3+4*5 -- > 23
(3+4)*5 -- > 35

-- Ex. Function Applications
abs 5 -- > 5
abs (-6) -- > 6

-- Ex. Function w/ Multiple Args
min 3 8 -- > 3
max 3 8 -- > 8

-- Ex. Precedence of Function Application (NOTE: fn application binds first!)
f x + 3 -- > (f x) + 3 (NOTE: vs. f (x + 3))


{-|
  -----------
  Equations
  -----------
-}

-- Ex. Equations give names to values
answer = 42

-- Ex. Equations are NOT assignments
{-
  NOTE: A name can be given only a single value, and while names are called
  'variables' they are immutable - meaning, 'pure' and 'no side effects'!
-}

n = 1 -- looks good!
x = 3*n -- again, no problem!
n = x -- NO! WRONG: Redefining definition of `n` not allowed

{-
  -------------------
  What about n = n+1?
  -------------------

  - This is categorically an assignment, no an equation, so this would fail
    were we to have Haskell compute the value

  - However, we could do something like this...
-}
inc :: Int -> Int
inc n = n+1


{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|

  λ 42
  42
  -- :: Num a => a
  λ 6*7
  42
  -- :: Num a => a
  λ 3+4*6
  27
  -- :: Num a => a
  λ 3+(4*6) == 3+4*6
  True
  -- :: Bool
  λ ((6))*(((7)))
  42
  -- :: Num a => a
  λ 4+-3
  -- Not in scope: `+-'
  -- Perhaps you meant one of these:
  -- `-' (imported from Prelude), `++' (imported from Prelude),
  -- `+' (imported from Prelude)
  λ 4+ -3
  -- Precedence parsing error
  -- cannot mix `+' [infixl 6] and prefix `-' [infixl 6] in the same infix expression
  λ 4+(-3)
  1
  -- :: Num a => a
  λ abs 7
  7
  -- :: Num a => a
  λ abs (-3)
  3
  -- :: Num a => a
  λ min 3 8
  3
  -- :: (Num a, Ord a) => a
  λ sqrt 9+7
  10.0
  -- :: Floating a => a
  λ sqrt (9+7)
  4.0
  -- :: Floating a => a
  λ min (max 3 4) 5
  4
  -- :: (Num a, Ord a) => a
  λ answer = 42
  42
  -- :: Num a => a
  λ answer
  42
  -- :: Num a => a
  λ double = answer * 2
  84
  -- :: Num a => a
  λ answer = answer * 2
  -- Evaluation killed!
  λ answer = 43
  -- Conflicting definitions for `answer'
  -- Bound at: <interactive>:1:6-11
  -- <interactive>:1:20-25
-}
