module Burrito.Internal.Type.Match where

import qualified Burrito.Internal.Type.Value as Value

newtype Match
  = Defined Value.Value
  deriving (Eq, Show)
