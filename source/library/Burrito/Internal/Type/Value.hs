{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Value
  ( Value(..)
  ) where

import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Represents a value that can be substituted into a template. Can be a
-- string, a list, or dictionary (which is called an associative array in the
-- RFC).
data Value
  = Dictionary (Map.Map Text.Text Text.Text)
  | List [Text.Text]
  | String Text.Text
  deriving (Data.Data, Eq, Ord, Show)
