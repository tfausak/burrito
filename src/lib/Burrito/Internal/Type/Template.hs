{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Template (Template(..)) where

import qualified Burrito.Internal.Type.Token as Token
import qualified Data.Data as Data

-- | Represents a URI template.
newtype Template = Template
  { tokens :: [Token.Token]
  } deriving (Data.Data, Eq, Ord, Show)
