{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Name where

import qualified Burrito.Internal.Type.Field as Field
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Data.Data as Data

newtype Name = Name
  { fields :: NonEmpty.NonEmpty Field.Field
  } deriving (Data.Data, Eq, Show)
