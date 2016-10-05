{-|
  ==============================================================================
  2.7 - Input and Output
  ==============================================================================
-}

{-|
  I/O, or input and output, is integral to successful computer programs - within
  a purely functional language like Haskell, input and output are slightly more
  complex, but still very possible and necessary. The basis of input and output
  in Haskell is built atop the mathematical endofunctor `monad.`
-}

-- Common I/O Operations
--------------------------------------------------------------------------------
getChar :: IO Char
putChar :: Char -> IO ()
main :: IO ()
ready :: IO Bool
return :: a -> IO a
getLine :: IO String


{-|
  NOTE: Below is the ghci output while experimenting with this course section
-}

{-|
  λ putStrLn "hello world"
  hello world
  -- :: IO ()
  λ putStrLn ("good " ++ "morning" ++ " everyone")
  good morning everyone
  -- :: IO ()
  λ getLine
  > Hello there, Haskeller!
  "Hello there, Haskeller!"
  -- :: IO String
  λ do { putStrLn "what is your name?"; x <- getLine; putStrLn ("hello " ++ x) }
  what is your name?
  > Marc
  hello Marc
  -- :: IO ()
  λ do { putStrLn "what is your name?"; n <- getLine; let nUpper = map toUpper n in putStrLn ("HELLO " ++ nUpper) }
  what is your name?
  > Marc
  HELLO MARC
  -- :: IO ()
  λ read "42" :: Int
  42
  -- :: Int
  λ read "42" :: Float
  42.0
  -- :: Float
  λ show 42
  "42"
  -- :: String
  λ putStrLn (show (6*7))
  42
  -- :: IO ()
  λ print 42
  42
  -- :: IO ()
-}


{-|
  ==============================================================================
  2.8 - I/O and a First Encounter with Monads
  ==============================================================================
-}

-- I/O is Impure
--------------------------------------------------------------------------------
-- Input and output is inheriently impure, they are the channels through which
-- actions dealing with the 'outside world' occur - that is, it is "the only
-- way to make computers do interesting things"

-- Sequencing Actions
--------------------------------------------------------------------------------
let greet() = do
    planet <- getLine
    home <- getLine
    putStrLn ("greetings " ++ planet ++ "ling.")
    putStrLn ("I am from " ++ home ++ ".")
    putStrLn "Take me to your leader."

let a = reverse "winston"
    b = reverse "churchill"
in "sir " ++ a ++ " " ++ b

-- NOTE: In the `let` block, the `=` operator associates pure function results
-- with a name, and in the `do` block the `<-` accomplishes the same.

-- NOTE: Note that in the above example, the order of the reversals does not
-- alter the result, because we are dealing with a pure function - in I/O
-- sequencing is vital.

-- The `do` notation is a command/keyword that allows us to "sequence" actions.
-- Though the `do` block looks like an imperative code block, underneath is
-- a series of functions calls where the output from the first becomes the
-- output of the next (i.e., currying). The `bind` operator does this via
-- function sequencing. More on monads and bind operators later....


{-|
  ==============================================================================
  2.11 - Guessing Game
  ==============================================================================
-}

-- NOTE: See project in current directory titled `/starman/`
