{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Field where

import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Language.Haskell.TH.Syntax as TH

newtype Field = Field
  { characters :: NonEmpty.NonEmpty (Character.Character Field)
  } deriving (Eq, TH.Lift, Show)
