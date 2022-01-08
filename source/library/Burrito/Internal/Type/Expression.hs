{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Expression
  ( Expression(..)
  ) where

import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Data.Data as Data
import qualified Data.List.NonEmpty as NonEmpty

data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  }
  deriving (Data.Data, Eq, Ord, Show)
