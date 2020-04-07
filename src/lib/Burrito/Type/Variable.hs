module Burrito.Type.Variable
  ( Variable(..)
  ) where

import qualified Burrito.Type.Modifier as Modifier
import qualified Burrito.Type.Name as Name


-- | Represents a variable in an expression.
data Variable = Variable
  { modifier :: Modifier.Modifier
  , name :: Name.Name
  } deriving (Eq, Show)
