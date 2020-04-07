module Burrito.Type.Modifier
  ( Modifier(..)
  ) where


-- | Represents a modifier on a variable. The number associated with a prefix
-- modifier will be between 1 and 9999 inclusive.
data Modifier
  = Asterisk
  | Colon Int
  | None
  deriving (Eq, Show)
