{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Expression where

import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Data.Data as Data

data Expression = Expression
  { operator :: Operator.Operator
  , variables :: NonEmpty.NonEmpty Variable.Variable
  } deriving (Data.Data, Eq, Show)
