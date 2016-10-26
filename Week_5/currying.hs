{-|
  ==============================================================================
  5.7 - Curry is on the menu
  ==============================================================================
-}

{-|
  ================================
  Partial Application and Currying
  ================================
-}

-- Currying
-- =============================================================================
-- Consider the following type signature
f :: X -> Y -> Z -> A

-- The arrow '->' is right associative, so this is the same as:
f :: x -> (Y -> (Z -> A))

-- What this means is we can regard the function f as a function with a single
-- arg of type X which returns a function of type Y -> Z -> A

-- The technique of rewriting a function of multiple args into a seq of fns
-- with a single arg is called 'currying'. We can illustrate it best with a
-- lambda function:
-- \x y z -> ...
-- \x -> (\y z -> ... )
-- \x -> (\y -> (\z -> ... ))

-- Though we call this 'currying' in honor of Haskell Curry, the concept was
-- first proposed by another logician, Moses Schoenfinkel

-- Partial application
-- =============================================================================
-- Partial application is when we don't need to provide all args to a function:
sq x y = x*x + y*y

-- We note that function applicaiton associates to the left, so the following
-- equivalence holds:
sq x y = (sq x) y

-- We can therefore create a specialised function by partial application of x:
sq4 - sq 4 -- = \y -> 16 + y*y
sq4 3 -- = (sq 4) 3 = sq 4 3 = 25

-- This is why we can write things like:
doubles = map (*2) [1 .. ]
