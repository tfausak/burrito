{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Character
  ( Character (..),
  )
where

import qualified Burrito.Internal.Type.Digit as Digit
import qualified Data.Data as Data

data Character tag
  = Encoded Digit.Digit Digit.Digit
  | Unencoded Char
  deriving (Data.Data, Eq, Ord, Show)
