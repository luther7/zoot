module Parser (
  parseValueExpression,
  parseQueryExpression
) where


import Syntax


import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (Operator(Infix, Prefix, Postfix), Assoc(AssocLeft, AssocRight, AssocNone), buildExpressionParser)
import Text.ParserCombinators.Parsec.Prim ((<|>), many, try)
import Text.ParserCombinators.Parsec.Char (oneOf, char, anyChar, digit, letter, satisfy, spaces)
import Text.ParserCombinators.Parsec.Combinator (many1, manyTill, choice, chainl1, between, sepBy, eof, optionMaybe)
import Control.Monad (void, guard)
import Data.Char (isLetter, isDigit)


--
--
--


makeQuery :: QueryExpression
makeQuery = Query {queryVerb = Nothing
                ,queryTarget = Nothing
                ,queryPreposition = Nothing
                ,queryCondition = Nothing
                }


--
-- Tokens
--


lexeme :: Parser a -> Parser a
lexeme l = l <* spaces

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    u <- many1 (oneOf "<>=+-^%/*!|")
    guard (s == u)
    return s

stringToken :: Parser String
stringToken = lexeme (char '\'' *> manyTill anyChar (char '\''))

dot :: Parser Char
dot = lexeme $ char '.'

comma :: Parser Char
comma = lexeme $ char ','

openParen :: Parser Char
openParen = lexeme $ char '('

closeParen :: Parser Char
closeParen = lexeme $ char ')'


--
-- Helpers
--


keyword :: String -> Parser String
keyword k = try $ do
    i <- identifier
    guard (i == k)
    return k

voidKeyword :: String -> Parser ()
voidKeyword = void . keyword

identifierBlacklist :: [String] -> Parser String
identifierBlacklist bl = do
    i <- identifier
    guard (i `notElem` bl)
    return i

parens :: Parser a -> Parser a
parens = between openParen closeParen

commaSeperated :: Parser a -> Parser [a]
commaSeperated = (`sepBy` comma)


--
-- Terms
--


stringLiteral :: Parser ValueExpression
stringLiteral = StringLiteral <$> stringToken

numberLiteral :: Parser ValueExpression
numberLiteral = NumberLiteral <$> integer

identity :: [String] -> Parser ValueExpression
identity blacklist = Identity <$> identifierBlacklist blacklist

parensValue :: Parser ValueExpression
parensValue = Parens <$> parens (valueExpression [])

term :: [String] -> Parser ValueExpression
term blackList = choice [identity blackList
                        ,numberLiteral
                        ,parensValue
                        ,stringLiteral]


--
-- Operators
--
table = [[prefix "-", prefix "+"]
         ,[binary "+" AssocLeft
          ,binary "-" AssocLeft]
         ,[binary "<=" AssocRight
          ,binary ">=" AssocRight
          ,binary "!=" AssocRight]
         ,[binary "<" AssocNone
          ,binary ">" AssocNone]
         ,[binary "=" AssocRight]
         ,[prefixKeyword "not"]
         ,[binaryKeyword "and" AssocLeft]
         ,[binaryKeyword "or" AssocLeft]]
  where
    binary name assoc =
        Infix (makeBinaryOperator name <$ symbol name) assoc
    makeBinaryOperator nm a b = BinaryOperator a nm b
    prefix name = Prefix (PrefixOperator name <$ symbol name)
    binaryKeyword name assoc =
        Infix (makeBinaryOperator name <$ keyword name) assoc
    prefixKeyword name = Prefix (PrefixOperator name <$ keyword name)


--
-- Value Expression
--


valueExpression :: [String] -> Parser ValueExpression
valueExpression blackList = buildExpressionParser table (term blackList)


--
-- Query Expression Parsing
--


verb :: Parser Verb
verb = choice
  [List <$ voidKeyword "list"
  ,Count <$ voidKeyword "count"
  ,Delete <$ voidKeyword "replace"
  ]

target :: Parser Target
target = choice
  [Files <$ voidKeyword "files"
  ,Dirs <$ voidKeyword "dirs"
  ]

preposition :: Parser Preposition
preposition = choice
  [In <$ voidKeyword "in"
  ,With <$ voidKeyword "with"
  ]

condition :: Parser Condition
condition = choice
  [Like <$ voidKeyword "like"
  ,Matching <$ voidKeyword "matching"
  ]


--
-- Query Expression
--


queryExpression :: Parser QueryExpression
queryExpression = Query
                  <$> optionMaybe verb
                  <*> optionMaybe target
                  <*> optionMaybe preposition
                  <*> optionMaybe condition


--
-- API
--


parseValueExpression :: String -> Either ParseError ValueExpression
parseValueExpression = parse (spaces *> valueExpression [] <* eof) ""

parseQueryExpression :: String -> Either ParseError QueryExpression
parseQueryExpression = parse (spaces *> queryExpression <* eof) ""
