{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Literal where

import qualified Burrito.Internal.Type.Character as Character
import qualified Data.Data as Data
import qualified Data.List.NonEmpty as NonEmpty

newtype Literal = Literal
  { characters :: NonEmpty.NonEmpty (Character.Character Literal)
  } deriving (Data.Data, Eq, Show)
