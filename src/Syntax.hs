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
  , queryTarget :: Maybe Target
  , queryPreposition :: Maybe Preposition
  , queryCondition :: [Condition]
  } deriving (Eq, Show)

data Verb
  = List
  | Count
  | Delete
  deriving (Eq, Show)

data Target
  = Files
  | Dirs
  deriving (Eq, Show)

data Preposition
  = In Target
  | With Target
  deriving (Eq, Show)

data Condition
  = Like ValueExpression
  | Matching ValueExpression
  | Where ValueExpression
  deriving (Eq, Show)
