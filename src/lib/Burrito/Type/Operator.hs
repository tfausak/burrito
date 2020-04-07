module Burrito.Type.Operator
  ( Operator(..)
  ) where


-- | Represents an operator in an expression.
data Operator
  = Ampersand
  | FullStop
  | None
  | NumberSign
  | PlusSign
  | QuestionMark
  | Semicolon
  | Solidus
  deriving (Eq, Show)
