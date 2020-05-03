{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Template where

import qualified Burrito.Internal.Type.Token as Token
import qualified Language.Haskell.TH.Syntax as TH

newtype Template = Template
  { tokens :: [Token.Token]
  } deriving (Eq, TH.Lift, Show)
