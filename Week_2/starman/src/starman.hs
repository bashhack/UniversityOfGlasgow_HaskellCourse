{-|
  ===============================================================================
  Starman Guessing Game:
  ===============================================================================

  GOALS:
  ======
  - Create a single-player, text-based guessing game, a la 'Hangman'

  GAME LOGIC:
  ===========
  - Each turn, a player guesses a single letter
  - If guess is correct, guessed letter(s) are shown in correct position
  - If guess is incorrect, user loses a star
  - If player has no stars, they have lost the game
  - If player has guessed all letters of the word, they have won the game
-}

{-|
  The 'check' function should evaluate the (1) secret word,
  (2) current display, and (3) the character guessed - returning a Boolean
  expression reflecting the correctness of the guess and a String,
  representing the new value to display to player
-}
check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x==c
                     then c
                     else y | (x,y) <- zip word display])

{-|
  The 'turn' function should be called each time the player must guess -
  at which point we must evaluate whether there are any guesses left,
  and whether the player's guess is correct
-}
turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn "You lose ()"
       else if word==display
              then putStrLn "You win!"
              else mkguess word display n

{-|
  The 'mkguess' function should take user input, pass value to 'check',
  and handle call to 'turn' if conditions are valid for a new turn
-}
mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ " " ++ take n (repeat '*'))
     putStrLn " Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

{-|
  The 'starman' function is our 'main' entry point, taking a word to guess
  and the number of incorrect guesses afforded to the player
-}
starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n
