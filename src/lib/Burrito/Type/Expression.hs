{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Expression
  ( Expression(..)
  )
where

import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Burrito.Type.Operator as Operator
import qualified Burrito.Type.Variable as Variable
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents an expression in a token.
data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  } deriving (Eq, TH.Lift, Show)
