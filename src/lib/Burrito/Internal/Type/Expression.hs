{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Expression where

import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Language.Haskell.TH.Syntax as TH

data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  } deriving (Eq, TH.Lift, Show)
