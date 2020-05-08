{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Match where

import qualified Burrito.Internal.Type.Value as Value
import qualified Data.Data as Data

data Match
  = Defined Value.Value
  | Undefined
  deriving (Data.Data, Eq, Ord, Show)
