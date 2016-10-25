{-|
  ==============================================================================
  4.11 - Using QuickCheck
  ==============================================================================
-}

-- QuickCheck is a tool in Haskell we can use to interact with our programs, to
-- debug and check the correctness of our code

-- We'll demonstrate how we might use QuickCheck with the `abs` function

import Test.QuickCheck
quickCheck ((\n -> (abs(n) == n))::Int->Bool)

-- When we run this code, it fails -- for negative numbers!

quickCheck ((\n -> (abs(n) == n) || (0-abs(n) == n))::Int->Bool)

-- Super! Now QuickCheck indicates success!

-- How about another example? This time let's check that the first element
-- of a sorted list is its minimum, or that the last element is its maximum

import Data.List
quickCheck ((\l -> ((minimum l) == (sort l)!!0))::[Int]->Bool)

-- Oops, we fail on an empty list!

quickCheck ((\l -> (if l == [] then True else (minimum l) == (sort l)!!0))::[Int]->Bool)

-- Great! Now we're rolling - to see the tests that were actually run, we can
-- use `verboseCheck` instead of `quickCheck`

-- This company uses QuickCheck at the center of its commercial product:
-- http://www.quviq.com/successes/
