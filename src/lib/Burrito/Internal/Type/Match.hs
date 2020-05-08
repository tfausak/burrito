module Burrito.Internal.Type.Match where

import qualified Burrito.Internal.Type.Value as Value

data Match
  = Defined Value.Value
  | Undefined
  deriving (Eq, Show)
