{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Field
  ( Field(..)
  ) where

import qualified Burrito.Internal.Type.Character as Character
import qualified Data.Data as Data
import qualified Data.List.NonEmpty as NonEmpty

newtype Field = Field
  { characters :: NonEmpty.NonEmpty (Character.Character Field)
  } deriving (Data.Data, Eq, Ord, Show)
