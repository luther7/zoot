module Parser
  ( Command,
    ParseError,
    parseCommand
  ) where


import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Prim (parse, try)
import Text.ParserCombinators.Parsec.Char (oneOf, char, digit, letter, satisfy, anyChar)
import Text.ParserCombinators.Parsec.Combinator (many1, manyTill, chainl1)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap, guard)
import Data.Char (isLetter, isDigit)


type Token = String

data Verb
  = List
  | Delete
  | Count
  deriving (Eq, Show, Read)

data Target
  = Dirs
  | Files
  | Lines
  | String
  deriving (Eq, Show, Read)

data Preposition
  = With
  | In
  deriving (Eq, Show, Read)

data Command
  = Verb
  | Target
  | Preposition
  deriving (Eq, Show)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

identifier :: Parser Command
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

keyword :: String -> Parser Command
keyword k = try $ do
  i <- identifier
  guard (i == k)
  return k

stringToken :: Parser Token
stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

verb :: Parser Command
verb = keyword "List"
--verb = keyword "List" <|> keyword "Delete" <|> keyword "Count"

-- target :: Parser Command
-- target = keyword "dirs"  <|> keyword "files"

-- preposition :: Parser Command
-- preposition = keyword "with"  <|> keyword "in"


command :: Parser Command
command = verb
-- command = verb <|> target <|> preposition <|> stringToken

parseCommand :: [String] -> Either ParseError Command
parseCommand (x:xs) = parse command "(Parse Command)" x
