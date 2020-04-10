{-# LANGUAGE DeriveLift #-}

module Burrito.Type.Modifier
  ( Modifier(..)
  ) where

import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a modifier on a variable. The number associated with a prefix
-- modifier will be between 1 and 9999 inclusive.
data Modifier
  = Asterisk
  | Colon Int
  | None
  deriving (Eq, TH.Lift, Show)
