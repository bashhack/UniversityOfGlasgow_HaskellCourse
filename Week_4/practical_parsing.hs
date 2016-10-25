module ShowParser ( parseShow ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

{-|
  ==============================================================================
  4.7 - Parsing using Parsec: A Practical Example
  ==============================================================================
-}

-- Parsing the output of derived Show
-- show :: Show a => a -> String
-- data D = D ... deriving (Show)
-- d :: D
-- d = D ...
-- str :: String
-- str = show d -- string representation of the instance of the data type

-- parseShow :: String -> String
-- xml = parseShow $ show res
-- ^ the `$` operator is for avoiding parentheses, anything appearing afterwards
--   will take precedence over anything that comes before

-- Let's start with defining our data type

data PersonRecord = MkPersonRecord {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]
} deriving (Show)

data Address = MkAddress {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

-- Let's create some instances of the PersonRecord
rec1 = MkPersonRecord
  "Marc Laughton"
  (MkAddress "Six Foot LLC" 1234 "W. Alabama" "Houston" "77057")
  102884
  [Green, Red]

rec2 = MkPersonRecord
  "Becky Laughton"
  (MkAddress "Xola" 5678 "N. Shepherd" "Houston" "77057")
  772021
  [Blue, Yellow]

-- Let's test our records:

main = putStrLn $ show [rec1, rec2]

-- The output I got when I ran this code (`runhaskell practical_parsing.hs`):
-- [MkPersonRecord {name = "Marc Laughton", address = MkAddress {line1 = "Six
-- Foot LLC", number = 1234, street = "W. Alabama", town = "Houston",
-- postcode = "77057"}, id = 102884, labels = [Green,Red]},MkPersonRecord
-- {name = "Becky Laughton", address = MkAddress {line1 = "Xola",
-- number = 5678, street = "N. Shepherd", town = "Houston", postcode = "77057"},
-- id = 772021, labels = [Blue,Yellow]}]

-- Building the parser
-- =============================================================================
-- Let's create a module `ShowParser` which exports a function `parseShow`

-- NOTE: See lines (1 - 5)

-- The parser
-- =============================================================================
-- The function `parseShow` takes output from `show` (a String) and produces
-- the corresponding XML (also a String).

parseShow :: String -> String
parseShow = run_parser showParser

showParser :: Parser String

run_parser :: Parser a -> String -> a
run_parser p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right val -> val

-- The XML format
-- =============================================================================

-- Header:
-- ^ <?xml version="1.0" encoding="utf-8"?>
xml_header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- Tags:
-- ^ <tag> ... </tag>

otag t = "<"++t++">"
ctag t = "</"++t++">"
tag t v = concat [otag t,v,ctag t]

-- Attributes:
-- ^ <tag attr1="..." attr2="...">
tagAttrs :: String -> [(String, String)] -> String -> String
tagAttrs t attrs v =
  concat [
         otag (unwords $ [t]++(map (\(k,v) -> concat [k,"=\"",v,"\""]) attrs))
         ,v
         ,ctag t]

-- We need some functions to join strings together, from the Prelude we use:
-- concat :: [[a]] => [a] -- join lists
-- unwords :: [String] -> String -- join words using spaces

-- We also define a function to join strings with newline characters:
-- joinNL :: [String] -> String -- join lines using "\"
-- ^ This is identical to `unlines` from the Prelude, just to illustrate the use
--   of `intercalate` and the `Data.List` module

-- Parsers for the derived Show format
-- =============================================================================

-- Lists
-- [ ..., ..., ... ]

-- XML:
-- <list>
-- <list-elt>...</list-elt>
-- ...
-- </list>

list_parser = do
  ls <- brackets $ commaSep showParser
  return $ tag "list" $ joinNL $ map (tag "list-elt") ls

-- Tuples
-- : ( ..., ..., ... )

-- XML:
-- <tuple>
-- <tuple-elt>...</tuple-elt>
-- ...
-- </tuple>

tuple_parser = do
  ls <- parens $ commaSep showParser
  return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

-- Record types
-- Rec { k=v, ... }

-- XML:
-- <record>
-- <elt key="k">v</elt>
-- ...
-- </record>

record_parser = do
  ti <- type_identifier
  ls <- braces $ commaSep kvparser
  return $ tagAttrs "record" [("name",ti)] (joinNL ls)

kvparser = do
  k <- identifier
  symbol "="
  t <- showParser
  return $ tagAttrs "elt" [("key",k)] t

type_identifier = do
  fst <- oneOf ['A' .. 'Z']
  rest <- many alphaNum
  whiteSpace
  return $ fst:rest

-- Algebraic data types
-- e.g. Label

-- XML:
-- <adt>Label</adt>

adt_parser = do
  ti <- type_identifier
  return $ tag "adt" ti

-- Quoted strings and numbers
quoted_string = do
  s <- stringLiteral
  return $ "\""++s++"\""

number = do
  n <- integer
  return $ show n

-- Complete parser
-- =============================================================================
-- We combine all parsers using the choice combinator <|>

showParser :: Parser String
showParser =
  list_parser <|> -- [ ... ]
  tuple_parser <|> -- ( ... )
  try record_parser <|> -- MkRec { ... }
  adt_parser <|> -- MkADT ...
  number <|> -- signed integer
  quoted_string <?> "Parse error"

{-|
  ==============================================================================
  Main Program
  ==============================================================================
-}

-- To use the parser, we would write:
rec_str = show [rec1,rec2]
main = putStrLn $ parseShow rec_str
