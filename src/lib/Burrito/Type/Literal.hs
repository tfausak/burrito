module Burrito.Type.Literal
  ( Literal(..)
  ) where

import qualified Burrito.Type.Character as Character
import qualified Burrito.Type.NonEmpty as NonEmpty


-- | Represents a literal in a token.
newtype Literal = Literal
  { characters :: NonEmpty.NonEmpty Character.Character
  } deriving (Eq, Show)
