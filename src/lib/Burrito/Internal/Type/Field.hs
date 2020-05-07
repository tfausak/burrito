{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Field where

import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Data.Data as Data

newtype Field = Field
  { characters :: NonEmpty.NonEmpty (Character.Character Field)
  } deriving (Data.Data, Eq, Show)
