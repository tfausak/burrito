module Burrito.Type.Template
  ( Template(..)
  ) where

import qualified Burrito.Type.Token as Token


-- | Represents a URI template.
newtype Template = Template
  { tokens :: [Token.Token]
  } deriving (Eq, Show)
