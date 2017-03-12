module Parser (
  parseExpression
) where


import Syntax


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Expr as Expression
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity


languageDefinition :: Token.LanguageDef ()
languageDefinition = Token.LanguageDef
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "#"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   =
    [ "nop"
    , "not"
    , "and"
    , "list"
    , "count"
    , "delete"
    , "files"
    , "dirs"
    , "lines"
    , "in"
    , "with"
    , "like"
    , "matching"
    ]
  , Token.reservedOpNames = []
  , Token.caseSensitive   = True
  }


lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDefinition

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

semicolonSeparated :: Parser a -> Parser [a]
semicolonSeparated = Token.semiSep lexer

reservedOperator :: String -> Parser ()
reservedOperator = Token.reservedOp lexer

binaryOperator :: String -> (a -> a -> a) -> Expression.Assoc -> Expression.Operator String () Identity a
binaryOperator s f c = Expression.Infix (reservedOperator s >> return f) c

prefixOperator :: String -> (a -> a) -> Expression.Operator String () Identity a
prefixOperator s f = Expression.Prefix (reservedOperator s >> return f)

postfixOperator :: String -> (a -> a) -> Expression.Operator String () Identity a
postfixOperator s f = Expression.Postfix (reservedOperator s >> return f)


table :: Expression.OperatorTable String () Identity Expression
table = [
    [
      prefixOperator "not" Not
    , binaryOperator "and" And Expression.AssocLeft
    ]
  ]


parseSubject :: String -> (Expression -> Expression) -> (String -> Expression) -> Parser Expression
parseSubject s f1 f2 = do
  reserved s
  _ <- char '"'
  regex <- many (noneOf "\"")
  _ <- char '"'
  return $ f1 (f2 regex)


parsePredicate :: String -> (Expression -> Expression) -> Parser Expression
parsePredicate s f = do
  reserved s
  remaining <- term
  return $ f remaining


term :: Parser Expression
term = parseSubject "like" Like Glob
  <|> parseSubject "matching" Matching Regex
  <|> parsePredicate "list" List
  <|> parsePredicate "count" Count
  <|> parsePredicate "delete" Delete
  <|> parsePredicate "files" Files
  <|> parsePredicate "dirs" Dirs
  <|> parsePredicate "in" In
  <|> parsePredicate "with" With
  <|> parens expression

expression :: Parser Expression
expression = Expression.buildExpressionParser table term

contents :: Parser a -> Parser a
contents p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r


parseExpression :: String -> Either ParseError Expression
parseExpression s = parse (contents expression) "<stdin>" s
