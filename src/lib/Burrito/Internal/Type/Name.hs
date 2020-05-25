{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Name (Name(..)) where

import qualified Burrito.Internal.Type.Field as Field
import qualified Data.Data as Data
import qualified Data.List.NonEmpty as NonEmpty

newtype Name = Name
  { fields :: NonEmpty.NonEmpty Field.Field
  } deriving (Data.Data, Eq, Ord, Show)
