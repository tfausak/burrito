{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Token where

import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Data.Data as Data

data Token
  = Expression Expression.Expression
  | Literal Literal.Literal
  deriving (Data.Data, Eq, Ord, Show)
