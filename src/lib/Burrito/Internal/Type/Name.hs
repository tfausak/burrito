{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Name where

import qualified Burrito.Internal.Type.Field as Field
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Language.Haskell.TH.Syntax as TH

newtype Name = Name
  { fields :: NonEmpty.NonEmpty Field.Field
  } deriving (Eq, TH.Lift, Show)
