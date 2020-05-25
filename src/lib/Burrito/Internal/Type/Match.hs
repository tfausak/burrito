{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Match (Match(..)) where

import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Data.Data as Data
import qualified Data.Text as Text

data Match
  = Defined Text.Text
  | Prefix MaxLength.MaxLength Text.Text
  | Undefined
  deriving (Data.Data, Eq, Ord, Show)
