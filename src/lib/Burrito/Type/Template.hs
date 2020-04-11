{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Template
  ( Template(..)
  ) where

import qualified Burrito.Type.Token as Token
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a URI template.
newtype Template = Template
  { tokens :: [Token.Token]
  } deriving (Eq, TH.Lift, Show)
