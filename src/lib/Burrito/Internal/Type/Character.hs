{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Character where

import qualified Burrito.Internal.Type.Digit as Digit
import qualified Language.Haskell.TH.Syntax as TH

data Character tag
  = Encoded Digit.Digit Digit.Digit
  | Unencoded Char
  deriving (Eq, TH.Lift, Show)
