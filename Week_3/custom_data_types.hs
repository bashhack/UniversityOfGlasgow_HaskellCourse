{-|
  ==============================================================================
  3.8 - Define Your Own Data Types
  ==============================================================================
-}

{-|
  -----
  Types
  -----
-}

-- A type is a set of values that are related - "a family of values"

-- Ex. Bool
-- "Within the family of type Bool, we have values of True and False"

-- Ex. Int
-- "Within the family of type Int, which consists of whole number values
-- from some minimum to some maximum (minBound :: Int / maxBound :: Int)"

-- More Examples:
`a` :: Char
[1,2,3] :: Int List
("victoria", 1837) :: (String, Int)
inc :: Int -> Int -- Function Type
inc n = n+1

{-|
  --------------------------------------------------
  Designing Our Own DataType (or, User-Defined Type)
  --------------------------------------------------
-}

-- We're going to create a data type to represent a lsit of alternative values,
-- inspired by the counting mechanism of indigenous peoples of the Amazon, where
-- they have "one, two, many"

data SimpleNum = One | Two | Many deriving Show
-- ^ Our data type and its values must be capitalized,
--   to be able to print we derive from class type Show

-- Within the ghci, we might then implement it as such:

-- Prelude> :set +m -- mutliline trigger
-- Prelude>
-- Prelude> let convert 1 = One
-- Prelude|     convert 2 = Two
-- Prelude|     convert _ = Many
-- Prelude|
-- Prelude> convert 1
-- One
-- Prelude> convert 300
-- Many
-- Prelude> map convert [1..5]
-- [One, Two, Many, Many, Many]
-- Prelude> :! clear

data CricketScore = Score [Char] Int Int deriving Show

Prelude> let x = Score "New Zealand" 350 4
Prelude|
Prelude> x
Score "New Zealand" 350 4
Prelude> :t x
x :: CricketScore

-- These types of custom data types are called 'algebraic data types',
-- In our examples, the alternative values example relates to 'algebraic sums'
-- and the record values example relate to 'algebraic products'


{-|
  ==============================================================================
  3.9 - Grow A Tree: Exploring the Binary Tree Data Type
  ==============================================================================
-}

-- The binary tree data type is often used for storing data in sorted order
-- to allow for efficient searching -- ex., a telephone directory

data Tree = Leaf | Node Int Tree Tree deriving Show
-- ^ NOTE: The type definition here is recursive given that Node has two
--         self-referential children

-- Ex. Simplest Tree
Leaf

-- Ex. Tree w/ Single Node
Node 3 Leaf Leaf

-- Let's write a function to compute the depth of a Tree
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- Now, write a function which traverses a tree and adds up all the values
-- Here is its type:
treeSum :: Tree -> Int

treeSum Leaf = 0
treeSum (Node x leftSubtree rightSubtree) =
  x + (treeSum leftSubtree) + (treeSum rightSubtree)

-- How about a function to check whether a tree is sorted properly?
-- NOTE: For any Node storing value x, all values in its left subtree
-- are < x and all values in its right subtree are >= x

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
  let leftSorted = isSortedTree leftSubtree minVal x
      rightSorted = isSortedTree rightSubtree x maxVal
  in x >= minVal && x < maxVal && leftSorted && rightSorted
