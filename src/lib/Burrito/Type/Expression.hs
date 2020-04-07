module Burrito.Type.Expression
  ( Expression(..)
  ) where

import qualified Burrito.Type.Operator as Operator
import qualified Burrito.Type.Variable as Variable
import qualified Data.List.NonEmpty as NonEmpty


-- | Represents an expression in a token.
data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  } deriving (Eq, Show)
