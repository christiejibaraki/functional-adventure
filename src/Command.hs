module Command where

import Text.Parsec hiding (parse, runParser, (<|>), sepBy1, choice)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import Data.Char

import Item
import Direction

-- functions to get Parsec's combinators to back trak
-- i.e. try all parsers even if one fails
(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

-- easier to use version of Parsec parser
parse :: Parser a -> String -> Either ParseError a
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Inspect [ItemName]
  | Drop [ItemName]
  | Take [ItemName]
  | Use [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

-- |itemNameP
-- Parse any string representing an ItemName into inhabitant of ItemName
itemNameP :: Parser ItemName
itemNameP = (choice $ map (\iname ->  (string (show iname)) *> pure iname)
                          itemNames)

-- |nounPhrase
-- parses a comma-separated list of nouns
-- accepts either no space at all between the comma separated items,
-- or one space after each comma
nounPhrase :: Parser [ItemName]
nounPhrase = sepBy1 itemNameP (string "," <* optional (string " "))

-- |inventoryP
-- only accepts the string 'inventory'
inventoryP :: Parser Command
inventoryP = do
  _ <- string "inventory"
  return Inventory

-- |lookP
-- only accepts the string 'look'
lookP :: Parser Command
lookP = do
  _ <- string "look"
  return Look

-- |inspectP
-- parses the word 'inspect' plus a noun phrase into a Command
-- there needs to be exactly one space between 'inspect'
-- and the first word of the noun phrase
inspectP :: Parser Command
inspectP = fmap Inspect (string "inspect" *> string " " *> nounPhrase)

-- |takeP
-- parses the word 'take' plus a noun phrase into a Command
-- there needs to be exactly one space between 'take'
-- and the first word of the noun phrase
takeP :: Parser Command
takeP = fmap Take (string "take" *> string " " *> nounPhrase)

-- |inspectP
-- parses the word 'use' plus a noun phrase into a Command
-- there needs to be exactly one space between 'use'
-- and the first word of the noun phrase
useP :: Parser Command
useP = fmap Use (string "use" *> string " " *> nounPhrase)

-- |dropP
-- parses the word 'drop' plus a noun phrase into a Command
-- there needs to be exactly one space between 'drop'
-- and the first word of the noun phrase
dropP :: Parser Command
dropP = fmap Drop (string "drop" *> string " " *> nounPhrase)

-- |directionP
-- expects a single lowercase word denoting a direction
directionP :: Parser Direction
directionP = do
  str <- string "north" <|> string "east" <|> string "west" <|> string "south"
  pure $ readDirection $ toUpper $ head str

-- |moveP
-- parses a move command
-- expects one of the four words, 'north', 'south', 'east', or 'west'
-- returns command telling the game to move in the relevant direction
moveP :: Parser Command
moveP = fmap Move directionP

-- |exitP
-- a parser for the command to quit the game
-- accepts either the single word 'quit' or the single word 'exit'
exitP :: Parser Command
exitP = do
  _ <- string "exit" <|> string "quit"
  pure Exit

-- |commandP
-- accepts any single command that is syntactically well-formed
-- returns the Command corresponding to the string in the language
commandP :: Parser Command
commandP = choice [inventoryP, lookP, inspectP,
                   takeP, dropP, useP, moveP, exitP]

-- |conjunctionP
-- parses a list of commands, separated by the word 'and' into a Conjunction
conjunctionP :: Parser Conjunction
conjunctionP = (sepBy1 (commandP) (string " and ")) <* eof

-- |parseInput
-- takes a string and returns the conjunction wrapped in a Just
parseInput :: String -> Maybe Conjunction
parseInput str =
  let result = parse conjunctionP str in
  case result of
    Right a -> Just a
    Left _ -> Nothing