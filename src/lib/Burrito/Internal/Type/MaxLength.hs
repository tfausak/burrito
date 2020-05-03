{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.MaxLength where

import qualified Language.Haskell.TH.Syntax as TH

newtype MaxLength = MaxLength
  { count :: Int
  } deriving (Eq, TH.Lift, Show)
