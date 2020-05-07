{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Literal where

import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Data.Data as Data

newtype Literal = Literal
  { characters :: NonEmpty.NonEmpty (Character.Character Literal)
  } deriving (Data.Data, Eq, Show)
