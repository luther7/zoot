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
  {queryVerb :: Maybe Verb
  ,queryTarget :: Maybe Target
  ,queryPreposition :: Maybe Preposition
  ,queryCondition :: Maybe Condition
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
  = In
  | With
  deriving (Eq, Show)

data Condition
  = Like
  | Matching
  deriving (Eq, Show)
