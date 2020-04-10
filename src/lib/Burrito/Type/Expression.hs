module Burrito.Type.Expression
  ( Expression(..)
  ) where

import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Burrito.Type.Operator as Operator
import qualified Burrito.Type.Variable as Variable


-- | Represents an expression in a token.
data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  } deriving (Eq, Show)
