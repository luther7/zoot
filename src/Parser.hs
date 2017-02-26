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


type Verb = String
type Target = String
type Preposition = String


data Command
  = Verb Target
  deriving (Eq, Show)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \r\n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

keyword :: String -> Parser String
keyword k = try $ do
  i <- identifier
  guard (i == k)
  return k

stringToken :: Parser String
stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

verb :: Parser String
verb = keyword "list"  <|> keyword "delete" <|> keyword "count"

target :: Parser String
target = keyword "dirs"  <|> keyword "files"

preposition :: Parser String
preposition = keyword "dirs"  <|> keyword "files"

-- command :: Parser String
-- command = (:) <$> stringToken

parseCommand :: [String] -> Either ParseError String
parseCommand (x:xs) = parse verb "(Parse Command)" x
