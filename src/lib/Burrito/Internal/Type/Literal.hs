{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Literal where

import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Language.Haskell.TH.Syntax as TH

newtype Literal = Literal
  { characters :: NonEmpty.NonEmpty (Character.Character Literal)
  } deriving (Eq, TH.Lift, Show)
