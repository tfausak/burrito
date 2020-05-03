{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Variable where

import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Language.Haskell.TH.Syntax as TH

data Variable = Variable
  { name :: Name.Name
  , modifier :: Modifier.Modifier
  } deriving (Eq, TH.Lift, Show)
