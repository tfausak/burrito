{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Case where

import qualified Language.Haskell.TH.Syntax as TH

data Case
  = Lower
  | Upper
  deriving (Eq, TH.Lift, Show)
