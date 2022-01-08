{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Variable
  ( Variable(..)
  ) where

import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Data.Data as Data

data Variable = Variable
  { name :: Name.Name
  , modifier :: Modifier.Modifier
  }
  deriving (Data.Data, Eq, Ord, Show)
