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
