{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Literal
  ( Literal(..)
  )
where

import qualified Burrito.Type.LitChar as LitChar
import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a literal in a token.
newtype Literal = Literal
  { characters :: NonEmpty.NonEmpty LitChar.LitChar
  } deriving (Eq, TH.Lift, Show)
