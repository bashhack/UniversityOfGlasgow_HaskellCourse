{-|
  ==============================================================================
  2.2 - Boolean Values and Expressions
  ==============================================================================
-}


{-|
  --------------
  Boolean Values
  --------------
-}

-- Operators testing the relationship between values, that is - using `==` we
-- check whether two values are equal - the return value from this operation
-- is generally a Boolean value (i.e, `true` or `false`)

-- Another function in Haskell that returns a Boolean is `elem` which tests
-- list membership

{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|
  λ 42==42
  True
  -- :: Bool
  λ 1 == 2
  False
  -- :: Bool
  λ 1 /= 2
  True
  -- :: Bool
  λ "hello" == "hola"
  False
  -- :: Bool
  λ "foo" /= "bar"
  True
  -- :: Bool
  λ True /= False
  True
  -- :: Bool
  λ True == 1
  -- No instance for (Num Bool) arising from the literal `1'
  -- In the second argument of `(==)', namely `1'
  -- In the expression: True == 1
  -- In the expression: let in True == 1
  λ 10 > 9
  True
  -- :: Bool
  λ [1,2,3] < [1,2,3,4]
  True
  -- :: Bool
  λ "Aardvark" < "Aaronic"
  True
  -- :: Bool
  λ elem 1 [1,2,3]
  True
  -- :: Bool
  λ 3 `elem` [1,2,3,4,5]
  True
  -- :: Bool
  λ 42 `max` 13
  42
  -- :: (Num a, Ord a) => a
  λ (+) 1 1
  2
  -- :: Num a => aλ 42==42
  True
  -- :: Bool
  λ 1 == 2
  False
  -- :: Bool
  λ 1 /= 2
  True
  -- :: Bool
  λ "hello" == "hola"
  False
  -- :: Bool
  λ "foo" /= "bar"
  True
  -- :: Bool
  λ True /= False
  True
  -- :: Bool
  λ True == 1
  -- No instance for (Num Bool) arising from the literal `1'
  -- In the second argument of `(==)', namely `1'
  -- In the expression: True == 1
  -- In the expression: let in True == 1
  λ 10 > 9
  True
  -- :: Bool
  λ [1,2,3] < [1,2,3,4]
  True
  -- :: Bool
  λ "Aardvark" < "Aaronic"
  True
  -- :: Bool
  λ elem 1 [1,2,3]
  True
  -- :: Bool
  λ 3 `elem` [1,2,3,4,5]
  True
  -- :: Bool
  λ 42 `max` 13
  42
  -- :: (Num a, Ord a) => a
  λ (+) 1 1
  2
  -- :: Num a => a
-}


{-|
  ==============================================================================
  2.3 - Zip Lists
  ==============================================================================
-}

{-|
  The `zip` function is used to combine a pair of lists into a list of pairs
-}

zip [1,2,3] [4,5,6]
-- > [(1,4),(2,5),(3,6)]
zip [1,2,3] "abc"
-- > [(1,'a'),(2,'b'),(3,'c')]
zip3 "glasgow" "beijing" "nairobi"
-- > [('g','b','n'),('l','e','a'),('a','i','i'),('s','j','r'),('g','i','o'),('o','n','b'),('w','g','i')]
length [1..10]
-- > 10
length ['a'..'z']
-- > 26
zip [1..10] ['a'..'z']
-- > [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]
-- ^ The length of the output is the length of the shorter of the two inputs
zipWith max [1,2,3] [0,2,4]
-- > [1,2,4]
zipWith (+) [1,2,3] [0,2,4]
-- > [1,4,7]
zipWith (\x ->(\y ->(x,y))) [1,2,3] "abc"
-- > [(1,'a'),(2,'b'),(3,'c')]

{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|
  λ not True
  False
  -- :: Bool
  λ not (not True)
  True
  -- :: Bool
  λ True && True
  True
  -- :: Bool
  λ False && True
  False
  -- :: Bool
  λ True || False
  True
  -- :: Bool
  λ False || False
  False
  -- :: Bool
  λ True `xor` False
  True
  -- :: Bool
  λ True `xor` True
  False
  -- :: Bool
  λ map (\inputs -> xor (fst inputs) (snd inputs)) [(x,y) | x <- [False,True], y <- [False, True]]
  [False,True,True,False]
  -- :: [Bool]
  λ and [False, True, False, True]
  False
  -- :: Bool
  λ or [False, True, False, True]
  True
  -- :: Bool
  λ if 2*2==4 then "happy" else "sad"
  "happy"
  -- :: [Char]
  λ if 1 then 0 else -1
  -- Could not deduce (Num Bool) arising from the literal `1'
  -- from the context (Num a)
  -- bound by the inferred type of it :: Num a => a at <interactive>:1:1
  -- In the expression: 1
  -- In the expression: if 1 then 0 else - 1
  -- In the expression: let in if 1 then 0 else - 1
  λ if False then 42 else "foo"
  -- No instance for (Num [Char]) arising from the literal `42'
  -- In the expression: 42
  -- In the expression: if False then 42 else "foo"
  -- In the expression: let in if False then 42 else "foo"
  λ if True then 42 else pi
  42.0
  -- :: Floating a => a
-}
