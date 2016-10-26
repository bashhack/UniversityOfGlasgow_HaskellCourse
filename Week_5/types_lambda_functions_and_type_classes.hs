{-|
  ==============================================================================
  5.6 - Types, lambda functions and type classes
  ==============================================================================
-}

{-
  ==============
  Function types
  ==============
-}


-- Ordinary data types are for primitives (like `Int` or `Char`) and basic data
-- structures (like `[Int]` and `[Char]`)

-- Algebraic data types are types that combine other types either as:

-- 1) records ('products')
data Pair = Pair Int Double

-- 2) variants ('sums')
data Bool = False | True

-- Functions have types containing an arrow (Int -> String)

-- ------------------
-- Lambda expressions
-- ------------------
-- Lambda expressions are critical in Haskell and in functional programming:

-- Named and anonymous expressions
-- =============================================================================
sum = 2 + 2 -- named
(-b) + sqrt (b^2 - 4*a*c) -- anonymous

-- Parts of the lambda expression
-- =============================================================================
-- \x -> e where x is an explicit statement that the formal parameter is x, and
-- the expression e that defines the value of the function

-- Anonymous functions
-- =============================================================================
-- A function can be defined and named using an equation:
f :: Int -> Int
f x = x+1

-- Since functions are first-class, it's often useful to denote a function
-- anonymously - again, we do this with lambda expressions
\x -> x+1

-- There may be any number of args:
\x y z -> 2*x + y*z

-- Using anonymous functions
-- =============================================================================
-- You can use an anonymous function anywhere you would use a function:
f = \x -> x+1

-- which is equivalent to:
f x = x+1

-- But, lambda expressions are most useful when they appear inside larger exprs:
map (\x -> 2*x + 1) xs


{-
  =====================================
  Monomorphic and polymorphic functions
  =====================================
-}

-- Monomorphic functions
-- =============================================================================
-- That is, to have one form:
f :: Int -> Char
f i = "abcdefghijklmnopqrstuvwxyz" !! i

x :: Int
x = 3

f x :: Char
f x -- > 'd'

-- Polymorphic functions
-- =============================================================================
-- That is, having many forms:
fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b


{-
  ========
  Currying
  ========
-}

-- Most programming languages allow functions to have any number of args
-- It turns out - this is unnecessary, we can restrict all function to have only
-- one arg without losing expressiveness

-- This process is called currying, honoring mathematician Haskell Curry
-- Higher order functions rely on this property
-- Its advantages are both practical and theoretical

-- A function with two arguments
-- =============================================================================
-- When we write a function like this:
f :: Int  -> Int -> Int
f x y = 2*x + y

-- it actually means this:
f :: Int -> (Int -> Int)

-- The function takes its arguments one at a time:
f 3 4 = (f 3) 4
g :: Int -> Int
g = f 3
g 10 -- > (f 3) 10 -- > 2*3 + 10

-- Grouping: arrow to the right, application left
-- =============================================================================
-- The arrow operator takes two types a -> b, and gives the type of a function
-- with argument type a and result type b

-- An application e1 e2 applies a function e1 to an argument e2

-- Note that for both types and applications, a function has only one arg

-- To make the notation work smoothly, arrows group to the right, and
-- application groups to the left
f :: a -> b -> c -> d
f :: a -> (b -> (c -> d))

f x y z = ((f x) y) z


{-
  ====================================
  Type classes and ad-hoc polymorphism
  ====================================
-}

-- The type of (+)
-- =============================================================================
-- Note that `fst` has the following type, and there is no restriction on what
-- types a and b could be:
fst :: (a,b) -> a

(+) :: Int -> Int -> Int
(+) :: Integer -> Integer -> Integer
(+) :: Ratio Integer -> Ratio Integer -> Ratio Integer
(+) :: Double -> Double -> Double

(+) :: a -> a -> a -- Nope! It has to be a number

-- Type classes
-- =============================================================================
-- (+) has type a -> a -> a for any type a that's a member of type class Num
(+) :: Num a => a -> a -> a

-- The class Num is a set of types for which (+) is defined
-- It includes Int, Integer, Double, etc.
-- But Num does not contain types like Bool,[Char], Int -> Double, etc.

-- Two kinds of polymorphism
-- =============================================================================

-- 1) parametric polymorphism
-- A polymorphic type that can be instantiated to any type
-- Represented by a type variable, it is convential to use a,b,c,...
-- Example: `length :: [a] -> Int` can take the length of a list whose
-- elements could have any type

-- 2) ad hoc polymorphism
-- A polymorphic type that can be instantiated to any type chosen from a set,
-- called a 'type class'
-- Represented by a type variale that is constrained using the => notation
-- Example: `(+) :: Num a => a -> a -> a` says that (+) can add values of
-- any type a, provided that a is an element of the type class Num

{-
  ==============
  Type inference
  ==============
-}

-- Type checking takes a type declaration and some code, and determines
-- whether the code actually has the type declared

-- Type inference is the analysis of code in order to infer its type

-- Type inference works by using a set of type inference rules, and combining
-- all the information obtained from the rules to produce the types


-- Type inference rules
-- =============================================================================
-- The type system contains a number of type inference rules, with the form:

     assumption -- what you're given
    ---------------------------------
    consequence -- what you can infer

-- Context
-- =============================================================================
-- Statements about types are written in the form similar to Γ ⊢ e :: α
-- This means "if you are given a set Γ of types, then it is proven that e has
-- type α"

-- Type of constant
-- =============================================================================

  c is a constant with a fixed type T
  -----------------------------------
             Γ ⊢ c :: T

-- If we know the type T of a constant c, then this is expressed by saying that
-- there is a given theorem that c :: T. Furthermore, this holds given any
-- context Γ


-- Type of application
-- =============================================================================

   Γ ⊢ e1 :: (α -> β)    Γ ⊢ e2 :: α
   -------------------------------
          Γ ⊢ (e1 e2) :: β
-- If e1 is a function with type a -> β, then the application of e1 to an arg
-- of type a gives the result of type β


-- Type of lambda expression
-- =============================================================================

       Γ,x :: α  ⊢  e :: β
    -------------------------
    Γ ⊢ (λx -> e) :: (α -> β)

-- We have a context Γ. Suppose that if we're also given that x :: a, then it can
-- be proven that an expression e :: β. Then we can infer that the function
-- λx -> e has type α -> β
