{-|
  ==============================================================================
  Parsing Text Using Higher-Order Functions
  ==============================================================================
-}


{-
  ===================
  The Parsing Problem
  ===================
-}

-- Parsing is a way of making sense of structured information - be it written
-- or spoken language. In the case of written language, parsing is composed
-- of several general steps:

-- 1) recognizing the characters of the writing system,
-- 2) identifying words,
-- 3) identifying sentences,
-- 4) identifying paragraphs, etc.

-- To accomplish these things, we must know the grammar of the language, writing
-- system, and spelling conventions

-- Parsing text such as program source code, HTML or JSON, is a similar problem.


{-
  ====================
  Functional Machinery
  ====================
-}

-- Returning functions as values
-- =============================================================================

-- Until this point, we've been looking at functions that take functions as args,
-- but functions can also return functions as values. Take, for example, partial
-- application:

sum = foldl (+) 0

-- We can write this a second way, as:
sum = \xs -> foldl (+) 0 xs

-- Here, sum is a function resulting from partial application of foldl

-- Function generators
-- =============================================================================

-- We can use this concept of functions returning other functions as values to
-- create parameterised functions

-- For example, here's how we might create a function generator that creates
-- functions that add a constant number to their argument:

gen_add_n = \n ->
  \x -> x+n

add_3 = gen_add_n 3
add_7 = gen_add_n 7

add_3 5 -- > 8
add_7 4 -- > 11

-- Of course, we can also go beyond numeric constants, here's how we might create
-- a function generator that performs a supplied arithmetic operation:

gen_op_n = \op n ->
  \x -> x `op` n

add_3 = gen_op_n (+) 3
mult_7 = gen_op_n (*) 7

add_3 5 -- > 8
mult_7 4 -- > 28


{-
  =================
  Practical Parsing
  =================
-}

-- Let's parse the following text:
-- =============================================================================

{-
  "Bring a large pot of water up to a boil. Unlike Italian pasta, you do not need
  to salt the water. Once it’s boiling, hold the noodles over the water and
  sprinkle them in strand by strand. Once all the noodles are in, stir gently so
  that they are all immersed in the water. Bring the water back up to a gentle
  boil, then lower the heat so that the water is just simmering. (This differs
  from the ’rolling boil’ that’s recommended for pasta.) If the water threatens
  to boil over, add about 1/2 cup of cold water (but if you lower the heat to the
  gentle simmer, and have a big enough pot, this shouldn’t be necessary). Cook
  for about 7 to 8 minutes, or following the package directions (for thinner
  noodles 5 to 6 minutes may be enough. Test by eating a strand - it should be
  cooked through, not al dente, but not mushy either)."
-}


-- Let's break down our methodology:
-- =============================================================================

-- 1) Typically, a functional program is organised around a tree-like structure
--    with an algebraic data type that represents the core data
-- 2) A parser will read the text input and generate the tree
-- 3) Functions perform transformations or traversals on the tree
-- 4) Pretty-printer functions output the tree (original or transformed)


-- Parsec: monadic parsing combinators
-- =============================================================================

-- Haskell has tons of parsing libraries
-- One of the most widely used is Parsec, which is robust, flexible, expressive,
-- and efficient
-- Parsec operates in a monad

-- Hey, Wait! What is a monad, again?
-- =============================================================================

-- We'll talk about monads in greater detail later - but - for now, let's just
-- say Haskell uses monads to structure computations. We've seen the IO monad,
-- which illustrates a few key syntactic features:
hello :: String -> IO String
hello x
  do
    putStrLn ("Hello, " ++ x)
    putStrLn "What's your name?"
    name <- getLine
    return name

-- 1) The `do` keyword
-- 2) The sequence of commands
-- 3) Extracting info from a monadic computation using the left arrow
--    `<-` and `return` keyword

-- Form of a parser
-- =============================================================================

-- Let's imagine we want to parse a string of the form - <tag> - where <tag>
-- must be a word, and return the tag as a type Tag
-- The general form of a parsing function might look like this:

data Tag = MkTag String

parseTag :: Parser Tag
parseTag =
  do char '<'
     x <- identifier
     char '>'
     return (MkTag x)

-- Here, the parser consists of a number of functions - i.e., `char` and
-- `identifier` that are called in sequence (in the monadic `do` block).
-- Also, the return value is of type `Parser Tag` not simply +Tag_.
-- This is because parseTag returns a new parser, not just a new value.
-- We can then combine this parser with other parsers, and execute the final
-- parser on our data.


-- Running the parser
-- =============================================================================
-- In this example, let's create a `parseDiv` function:

-- the "deriving Show" is needed to let `ghci` print the result
data Tag = MkTag String deriving Show

parseDiv = do
  string "<div>"
  return (MkTag "div")

-- We could call the function like so:
let parseDiv = do { string "<div>";return $ MkTag "div" }

-- We would apply the function as follows:
-- Prelude Text.ParserCombinators.Parsec> parseTest parseDiv "<div"
-- MkTag "div"


-- Anatomy of a basic parser
-- =============================================================================
-- The basic parsers (identifier), (natural), (char) take no args
-- (identifier) or one or more strings for parametrisation (char)

char = \ch -> \str ->
  -- try to match the character ch
  -- return the result


-- Anatomy of a parser combinator
-- =============================================================================
-- Parser combinators such as < | > and parens take other parsers as args
parens = \p ->
  \str ->
    -- first match "("
    -- perform the parse of p if "(" was found
    -- then match ")"
    -- return the result

-- Parsing alternatives
-- =============================================================================
-- When we want to try one parser, and in the event of failure, try another one.
-- THe choice combinator < | > provides this functionality.

letter_digit :: Parser Char
letter_digit =
  do x <- letter <|> digit
     return x

-- Parsing alternatives strings
-- =============================================================================
-- If we wanted to match one string or another, but nothing else:

bag_bog :: Parser String
bag_bog =
  do xs <- try (string "bag") <|> string "bog"
  -- ^ using try here allows us to parse optimistically, without consuming any
  --   input, this prevents a non-matching string failure to be consumed and
  --   prevent future correct batches from correctly registering
     return xs

-- Prelude Text.ParserCombinators.Parsec> parseTest bag_bog "bag"
-- "bag"

-- Prelude Text.ParserCombinators.Parsec> parseTest bag_bog "bag"
-- "bag"

-- Some parsers from the library
-- =============================================================================
-- (char\; "?") -- (char) is applied to a character, and it gives a parser that
-- matches that character
-- (letter) -- matches any letter
-- (digit) -- matches any digit
-- (string) -- matches a string of characters
-- (stringLiteral\; "xyz*") -- matches the string argument
-- (many\; p) -- matches 0 or more occurences of parser (p)
-- (many1\; p) -- matches 1 or more occurrences of parser (p)

-- Variable names
-- =============================================================================
varname :: Parser String
varname =
  do x <- letter
     xs <- many (letter <|> digit)
     return (x:xs)

-- Prelude Text.ParserCombinators.Parsec> parseTest varname "a4cc7*5"
-- "a4cc7"
-- Prelude Text.ParserCombinators.Parsec> parseTest varname "34a"
-- parse error at (line 1, column 1):
-- unexpected "3"
-- expecting letter

-- Expression parsers
-- =============================================================================
-- Arithmetic expressions are difficult to parse because of the arity of
-- operators and rules of precedence

-- Parsec provides support for expression parsing, so you don't have to reinvent
-- the wheel!

expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

optable =
  let
    op name assoc =
      Infix ( do { reservedOp name;
                 return (\x y -> (Op (MkOpExpr name x y))) } ) assoc
    prefix name =
      Prefix ( reservedOp name >>
               return (\x -> (Pref (MkPrefixOpExpr name x))) )
  in
    [ [ op "*" AssocLeft, op "/" AssocLeft, op "%" AssocLeft ]
    , [ op "+" AssocLeft, op "-" AssocLeft ], [ prefix "-" ] ]

-- There is some monad syntax here, and a demonstration of using braces instead
-- of indentation, and the `>>` operator in place of the `do` notation

do
  expr1
  expr2

-- is the same as:

expr1 >> expr2

-- <?> is used to define a custom error message in case a parse fails without
-- comsuming any input - this can be a very useful debugging feature!
