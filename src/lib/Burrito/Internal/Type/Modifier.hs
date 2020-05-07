{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Modifier where

import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Data.Data as Data

data Modifier
  = Asterisk
  | Colon MaxLength.MaxLength
  | None
  deriving (Data.Data, Eq, Show)
