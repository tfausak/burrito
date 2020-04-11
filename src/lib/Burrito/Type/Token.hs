{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Token
  ( Token(..)
  )
where

import qualified Burrito.Type.Expression as Expression
import qualified Burrito.Type.Literal as Literal
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a token in a template.
data Token
  = Expression Expression.Expression
  | Literal Literal.Literal
  deriving (Eq, TH.Lift, Show)
