module Syntax where

data Expression
  = Nop
  | Glob String
  | Regex String
  | Condition Bool
  | And Expression Expression
  | Not Expression
  | List Expression
  | Count Expression
  | Delete Expression
  | Files Expression
  | Dirs Expression
  | Lines Expression
  | In Expression
  | With Expression
  | Like Expression
  | Matching Expression
  deriving Show
