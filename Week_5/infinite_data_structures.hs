{-|
  ==============================================================================
  5.3 - Infinite Data Structures
  ==============================================================================
-}

-- Because Haskell implements lazy evaluation (vs eager evaluation), we can work
-- with infinite data structures easily.

-- To define an infinite list of consecutive integers:
[1..]

-- To define an infinite list comprised of identical values:
repeat 'a'

-- We can't print out the list in its entirety -- because it goes on forever.
-- We can isolate values from the list with functions like `take` or `drop`.

-- Now, let's look at two famous infinite integer lists.

{-
  =================
  Fibonacci Numbers
  =================

  The nth Fibonacci number is the sum of the previous two Fibonacci nums, ex.
  `1, 1, 2, 3, 5, 8, 13, 21, ...
-}

let fibs = 1:1:(zipWith (+) fibs (tail fibs))

-- To grab an individual element, we could use the `!!` operator to get the
-- indexed list selection

{-
  =============
  Prime Numbers
  =============
-}

-- Below, a series of `filter` exprs to calculate an infinite list of primes:

properfactors :: Int -> Int
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..]
