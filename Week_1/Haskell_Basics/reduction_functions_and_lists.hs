{-|
  ==============================================================================
  1.7 - More Basic Elements By Example
  ==============================================================================
-}


{-|
  ------
  Blocks (`let` expressions)
  ------
-}

-- Ex. An example of block scope in JavaScript (i.e., functions are blocks)
-- function roots(a, b, c) {
--   determinate = sqrt(b*b-4*a*c);
--   root1 = (-b + determinate)/a/2;
--   root2 = (-b - determinate)/a/2;
--   return [root2, root1];
-- }

-- Ex. An example of defining block scope in Haskell with let ... in ...
roots a b c =
  let
    determinate = sqrt(b*b-4*a*c)
    root1 = (-b + determinate)/a/2
    root2 = (-b - determinate)/a/2
  in
    [root2,root1]


{-|
  ----------
  Conditions
  ----------
-}

-- Ex. A condition in Python to get max from a pair of integers
-- def max(x, y):
--     if x > y:
--         return x
--     else:
--         return y

-- Ex. Haskell condition construct (NOTE: if...then....else... is an expression,
--     so it must return a value)
max x y =
  if x > y
  then x
  else y

max a b =
  do
    if a > b
      then a
      else b

{-|
  ---------------
  Case Statements
  ---------------
-}

-- Ex. A `case` statement in Ruby
-- Red = 1
-- Blue = 2
-- Yellow = 3

-- color = set color();
-- action case color
--   when Red then action1()
--   when Blue then action2()
--   when Yellow then action3()
-- end

-- Ex. Haskell case statement
data Color = Red | Blue | Yellow
-- ^ NOTE: We use the type as the value to decide on case
color = set_color
action = case color of
  Red -> action1
  Blue -> action2
  Yellow -> action3


{-|
  ------------------
  Generics/Templates
  ------------------
-}

-- Ex. Simple C++/Java example of generic data types (aka template type)
-- Map<String,Integer> set = new HashMap<String,Integer>();
-- To use the code above, we might say:
-- set.put("Answer",42)

-- We would accomplish the same thing in Haskell by writing:
set :: Data.Map.Map String Integer
set = Data.Map.empty

set' = insert "Answer" 42 set


{-|
  ==============================================================================
  1.8 - Reduction, Functions and Lists
  ==============================================================================
-}


{-|
  ---------
  Reduction
  ---------
-}


-- Program Execution Model
--------------------------------------------------------------------------------
-- 1) Reduction is a model of program execution
-- 2) In imperative programming, we can work the stack (the values of our
--    variables) and where we are at any given point in the program execution (
--    via the program counter) because the model of program execution is
--    based on statements - however, in Haskell, as with other purely
--    functional languages, we do not have statments!
-- 3) Haskell uses `reduction` as its means of program execution


-- Reduction
--------------------------------------------------------------------------------
-- 1) ..."is the process of converting an expression to a simpler form"
-- 2) Whereas in imperative programming paradigms, we used the
--    statment-by-statement model of program execution, reduction occurs
--    reducible expression-by-reducible expression. Each step, or
--    reducible expression ("redex") transforms the expression into simpler one

-- Ex. Reduction in Action!
3 + (4*5)
-- >
3 + 20
-- >
23

-- Unique Reduction Path
--------------------------------------------------------------------------------
-- 1) When reduction occurs, there is ever only one possible answer, though
--    there may be multiple paths - in the example below, we have a single
--    reduction path where each step only contains a single redex

-- Ex. Single path redex
3 + (5 * (8-2))
-- >
3 + (5 * 6)
-- >
3 + 30
-- >
33


-- Multiple Reduction Path
--------------------------------------------------------------------------------
-- 1) If an expression has several redexes, there will multiple reduction paths

(3+4) * (15-9)
-- >
7 * (15-9) -- or, (3+4) * 6
-- >
7 * 6
-- >
42

{-|
  ------------------------------------------------------------------------------
  HERE'S THE KICKER! REDUCTION RESULT DOES NOT DEPEND ON REDUCTION PATH!
                                                       -- Church-Rosser theorem
  ------------------------------------------------------------------------------

  -- This means that:
  -- 1) Correctness isn't dependent on order of evaluation, which in turn means,
  -- 2) the compiler can change the order to improve performance, without
  --    affecting the result,
  -- 3) and that different expressions can be evaluated in parallel, without
  --    affecting the result - can anyone say, "parallel systems programming!"
-}


{-|
  ---------
  Functions
  ---------
-}

-- A simple definition of a function: a relation between a set of inputs
-- and a set of permissible outputs, such that, given the same args the function
-- will return the same result

-- Functions have two primary operations: function definition (function creation)
-- and function application (using a function to compute a result)


-- Defining A Function
--------------------------------------------------------------------------------
-- 1) Haskell's standard lib functions are called "prelude"
-- 2) At its essence, a function is an "equation":
f = \x -> x+1 -- lambda function
-- or
f x = x+1 -- named function

-- The lefthand side of the function equation is a variable and the righthand
-- side is an expression that uses the local variable and defines the result
-- of the expression


-- Function Application
--------------------------------------------------------------------------------
-- 1) The right hand side of our function equation (known as the body) is an
--    expression giving the formal params, and the value of the application
-- 2) Function application is an expression where the function equation is
--    called with some or all of its arguments
f = \x -> x+1
f 31
-- 3) The application is evaluated by replacing it with the body of the function,
--    where formal params are replaced by the args
f = \x -> x+1
f 31
-- >  {bind x=31}
(x+1) where x=31
-- > {substitute 31 for x}
31+1
-- >
32


-- Multiple Arguments and Results
--------------------------------------------------------------------------------
-- Ex. A curried function
add3nums = \x y z -> x + y + z

10 + 4* add3nums 1 2 3
=
  10 + (4*(1+2+3))
  -- >
  10 + (4*6)
  -- >
  10 + 24
  -- >
  34


{-|
  -----
  Lists
  -----
-}

-- Like functions, lists are a key part of the Haskell language and one of the
-- most important data structures in function programming languages

-- A list is a collection and value that contains several other values
['5', 'c']
[5.308, 24.0, -1.0]

-- Because functions return only a single result, lists are a convenient
-- way for us to package up several values into a single object, able to
-- be a return value of that function
minmax = \x y -> [min x y, max x y]
minmax 3 8 -- > [3,8]
minmax 8 3 -- > [3,8]

-- Haskell uses lazy evaluation, so for the following:
answer = 42
yourlist = [7, answer+1, 7*8]

-- The inner expressions (i.e., answer+1 and 7*8) are NOT evaluated until
-- the moment they are accessed, like so:
yourlist -- > [7, 42, 56]


-- Contructing Lists
---------------------------------------------------------------------------------
-- The `++`, or "append", operator takes two existing lists and returns a new one
[23, 29] ++ [48, 41, 44] -- > [23, 29, 48, 41, 44]

-- Remember, the "length of the result is always the sum of the lengths of the
-- original lists"
-- If `xs` is a list, then []++xs = xs = xs++[]

-- We could also utilize the `:` operator, or "cons" operator
23 : [48,41,44]
-- > [23,48,41,44]
42 : []
-- > [42]
-- NOTE: The cons operator is usually used in a recursive fashion


-- Sequences
---------------------------------------------------------------------------------
-- When we need a sequence of numbers, or any enumerable type, we can use
-- Haskell's standard notation for lists:
[0..5] -- > [0, 1, 2, 3, 4, 5]
[100..103] -- > [100, 101, 102, 103]
['a'..'z'] -- > ['a', 'b', 'c', 'd', 'e', .... 'z']
['0'..'9'] -- > ['0', '1', '2', '3', .... '9']


-- List Comprehensions
--------------------------------------------------------------------------------
-- 1) List, like map or other higher-order functions, is a high level operation
--    specifying computation of a list
-- 2) The compiler transforms list comprehensions into an expression using basic
---   functions that operate on lists
-- 3) List comprehensions are inspired by maths "set comprehension"

-- Ex. Maths set comprehension (written in maths notation)
-- {3 x X | X <- {1,...,10}}
-- {2 x X | X <- N}
-- {2 x X + 1 | X <- N}
-- {(a,b) | a <- A, b <- B}

-- Ex. Haskell list comprehensions
[3*x | x <- [1..10]]
[2*x | x <- [0..10]]
[2*x + 1 | x <- [0..10]]
[[a,b] | a <- [10,11,12], b <- [20,21]]
-- > [[10,20],[10,21],[11,20],[11,21],[12,20],[12,21]]

-- Operating On Lists
--------------------------------------------------------------------------------
-- 1) When indexing a list, we use the `!!` operator which takes a list and idx:
[5,3,8,7] !! 2 -- > 8
[0 .. 100] !! 81 -- > 81
['a' .. 'z'] !! 'n' -- > 'n'

-- 2) If the index is negative, or too large, the returned value is `undefined`
-- 3) For code security, all expressions should be defined, and all exceptions
--    caught and handled


-- `head` and `tail`
--------------------------------------------------------------------------------
-- These are standard lib functions, they can sometimes be useful, but - as
-- we want to avoid `undefined` values in our code, err on the side of caution
-- and use pattern matching unless you really, really, really need these util
-- functions
head :: [a] -> a
head [4,5,6] -- > 4

tail :: [a] -> [a]
tail [4,5,6] -- > [5,6]

{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|
  λ f x = x+1
  -- :: ? -> ?
  λ f 3
  4
  -- :: Num a => a
  λ add3nums x y z = x + y + z
  -- :: ? -> ?
  λ 10 + 4* add3nums 1 2 3
  34
  -- :: Num a => a
  λ (\x -> x+1) 4
  5
  -- :: Num a => a
  λ f1 = \x -> x+1
  x + 1
  -- :: Expr
  λ f1 4
  5
  -- :: Num a => a
  λ add3nums1 = \x y z -> x + y + z
  x + y + z
  -- :: Expr
  λ 10 + 4* add3nums1 1 2 3
  34
  -- :: Num a => a
  λ [1,2,4,8]
  [1,2,4,8]
  -- :: Num t => [t]
  λ ["A","list","of","strings"]
  ["A","list","of","strings"]
  -- :: [[Char]]
  λ length ["A","list","of","strings"]
  4
  -- :: Int
  λ sumprod = \x y -> [x+y,x*y]
  [x + y,x * y]
  -- :: [Expr]
  λ sumprod 3 8
  [11,24]
  -- :: Num t => [t]
  λ mylist = [2,4,6,8]
  [2,4,6,8]
  -- :: Num t => [t]
  λ answer = 42
  42
  -- :: Num a => a
  λ yourlist = [7, answer+1, 7*8]
  [7,43,56]
  -- :: Num a => [a]
  λ yourlist
  [7,43,56]
  -- :: Num a => [a]
  λ 23 : [48,41,44]
  [23,48,41,44]
  -- :: Num a => [a]
  λ 42 : []
  [42]
  -- :: Num a => [a]
  λ [23,29] ++ [48,41,44]
  [23,29,48,41,44]
  -- :: Num a => [a]
  λ length [23,29] + length [48,41,44] == length ([23,29] ++ [48,41,44])
  True
  -- :: Bool
  λ [0 .. 5]
  [0,1,2,3,4,5]
  -- :: (Enum t, Num t) => [t]
  λ [1.1,1.2 .. 2.0]
  [1.1,1.2,1.2999999999999998,1.3999999999999997,1.4999999999999996,1.5999999999999994,1.6999999999999993,1.7999999999999992,1.899999999999999,1.999999999999999]
  -- :: (Enum t, Fractional t) => [t]
  λ ['a', 'd' .. 'z']
  "adgjmpsvy"
  -- :: [Char]
  λ [3*x | x <- [1 .. 10]]
  [3,6,9,12,15,18,21,24,27,30]
  -- :: (Enum t, Num t) => [t]
  λ [2*x+1 | x <- [0,2 ..10]]
  [1,5,9,13,17,21]
  -- :: (Enum t, Num t) => [t]
  λ [[a,b] | a <- [10,11,12], b <- [20,21]]
  [[10,20],[10,21],[11,20],[11,21],[12,20],[12,21]]
  -- :: Num t => [[t]]
  λ [5,3,8,7] !! 2
  8
  -- :: Num a => a
  λ ['a' .. 'z'] !! 33
  -- *Exception: Prelude.!!: index too large
  λ head [4,5,6]
  4
  -- :: Num a => a
  λ tail [4,5,6]
  [5,6]
  -- :: Num a => [a]
  λ head [] :: Int
  -- *Exception: Prelude.head: empty list
  λ tail [] :: [Int]
  -- *Exception: Prelude.tail: empty list
-}
