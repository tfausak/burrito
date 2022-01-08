{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Case
  ( Case(..)
  ) where

import qualified Data.Data as Data

data Case
  = Lower
  | Upper
  deriving (Data.Data, Eq, Ord, Show)
