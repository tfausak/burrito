{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Value where

import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Text as Text

data Value
  = Dictionary (Map.Map Text.Text Text.Text)
  | List [Text.Text]
  | String Text.Text
  deriving (Data.Data, Eq, Ord, Show)
