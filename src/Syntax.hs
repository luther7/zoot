module Syntax where


data ValueExpression
  = StringLiteral String
  | NumberLiteral Integer
  | Identity String
  | PrefixOperator String ValueExpression
  | BinaryOperator ValueExpression String ValueExpression
  | Parens ValueExpression
  deriving (Eq, Show)

data QueryExpression
  = Query
  { queryVerb :: Maybe Verb
  , queryNoun :: Maybe Noun
  , queryPreposition :: Maybe Preposition
  , queryCondition :: [Condition]
  } deriving (Eq, Show)

data Verb
  = List
  | Count
  | Delete
  deriving (Eq, Show)

data Noun
  = Files [Condition]
  | Dirs [Condition]
  deriving (Eq, Show)

data Preposition
  = In Noun
  | With Noun
  deriving (Eq, Show)

data Condition
  = Like ValueExpression
  | Matching ValueExpression
  | Where ValueExpression
  deriving (Eq, Show)
