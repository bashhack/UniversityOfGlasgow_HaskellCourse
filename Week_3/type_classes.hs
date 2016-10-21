{-|
  ==============================================================================
  3.10 - Type Classes
  ==============================================================================
-}


-- We've been working with types since the outset of our studies with Haskell,
-- but we've largely glossed over the more complex parts of the language in the
-- process. Let's look now at one of those complexities: `type classes`

-- Examples of some types we've encountered:
-- Int
-- Float
-- Infinite Precision Integers
-- Char

-- NOTE: To check the type of a value in GHCi, type: `:t value`

-- Let's break down a type, in this case, the type of add:
-- `:t (+)`

-- Prelude> :type (+)
-- (+) :: Num a => a -> a -> a

-- "Given a type 'a' (alpha), belonging to the Num type class,
-- then the plus function takes two 'a' parameters and returns
-- an 'a' result - 'a' is a 'type variable'"

-- "The part of the type signature to the left of the fat arrow
-- (in this example, `Num a`)is called the 'context,' this expresses
-- type class membership for type variables"

-- Prelude> :type (==)
-- (==) :: Eq a => a -> a -> Bool
-- Prelude> 1 == 1
-- True

-- Prelude> :type (<)
-- (<) :: Ord a => a -> a -> Bool
-- Prelude> "aaron" < "aardvark"
-- False

-- NOTE: "The Ord type class requires members to implement
-- functions like `<` and `>` - in the above example, Haskell
-- uses lexicogrpahic order for String values

-- NOTE: "The Show type class requires members to implement the show function,
-- that is, we can generate string values that represent values of such types,
-- like toString() in JavaScript."

-- Going back to our custom data type SimpleNum
data SimpleNum = One | Two | Many deriving (Show, Read, Eq)
-- ^ Here we are deriving from multiple classes (Show indicates we can convert
--   to character strings (typically for I/O), and Read provides operations for
--   parsing character strings to obtain the values they may represent)

-- Fun Fact!
-- Type classes were one of the earliest innovations of the Haskell language

{-|
  ==============================================================================
  Summary
  ==============================================================================
-}

-- 1. A type class constrains member types (instances) to conform to an API
-- 2. Type classes are like interfaces in C# and Java
-- 3. Types in the type class are like concrete implementations of interfaces
-- 4. Type classes enable operator overloading
