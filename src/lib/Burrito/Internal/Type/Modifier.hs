{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Modifier where

import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Language.Haskell.TH.Syntax as TH

data Modifier
  = Asterisk
  | Colon MaxLength.MaxLength
  | None
  deriving (Eq, TH.Lift, Show)
