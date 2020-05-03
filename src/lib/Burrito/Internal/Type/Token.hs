{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Token where

import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Language.Haskell.TH.Syntax as TH

data Token
  = Expression Expression.Expression
  | Literal Literal.Literal
  deriving (Eq, TH.Lift, Show)
