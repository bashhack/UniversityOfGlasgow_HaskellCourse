{-|
  ==============================================================================
  4.2 - Keep Your Programs Tidy
  ==============================================================================
-}

-- We keep our programs tidy by scoping intelligently, we seek to limit the
-- the region of the program in which names might 'exist' and can be used.

-- In Haskell, one way of scoping is via `let` which provides local scope.
-- A `let` expression has "a series of equations defining variable values and
-- a final expression (after the `in` keyword) that computes a value with
-- those variables in scope"

let x = 2
in x * x

let x = 2
    y = 3
in x + y

journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre =
  let milespergallon = 35
      litrespergallon = 4.55
      gallons = miles/milespergallon
  in (gallons * litrespergallon * fuelcostperlitre)

let diameter = 2 * radius
    circumference = pi * diameter
in (diameter, circumference)

-- We can also introduce local variables using the `where` clause

squareplusone :: Int -> Int
squareplusone x = xsquared + 1
  where xsquared = x * x

-- Like `let`, we can have multiple variables inside a `where` clause

cel2fahr :: Float -> Float
cel2fahr x = (x * scalingfactor) + freezingpoint
  where scalingfactor = 9.0/5.0
        freezingpoint = 32

-- Though `let` and `where` are very similar, a `let` expression is a
-- true expression and can be used anywhere an expression is allowed,
-- while a `where` clause is not an expression and can be used to provide
-- some local variables to a top level equation


{-|
  ==============================================================================
  4.3 - Guards, Guards
  ==============================================================================
-}

-- Haskell provides notation for defining functions based on predicate values:
f x
  | predicate1 = expression1
  | predicate2 = expression2
  | predicate3 = expression3

-- The above uses guards, but let us provide another means of visualizing them:
absolute x = if (x<0) then (-x) else x

-- Now, with guards:
absolute x
  | x<0 = -x
  | otherwise = x

-- Elsewhere, we have another example:
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show (score) ++ " over par"
  where score = strokes-par

{-
  ================
  Case expressions
  ================
-}

-- The `case` expression examines the value, and chooses the corresponding clause.
-- It's pattern matching, like a switch statement in JavaScript

data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name

hello (Parrot "polly") -- "pretty polly"

-- Just like the `otherwise` keyword is a catch all pattern for a case, the
-- underscore character, `_`, signifies 'don't care' or 'match anything'

hello :: Pet -> String
hello x =
  case x of
    Parrot name -> "pretty " ++ name
    _ -> "grunt"


{-|
  ==============================================================================
  4.4 - Dealing with Uncertainty
  ==============================================================================
-}
