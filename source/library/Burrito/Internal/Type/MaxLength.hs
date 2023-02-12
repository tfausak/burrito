{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.MaxLength
  ( MaxLength (..),
  )
where

import qualified Data.Data as Data

newtype MaxLength = MaxLength
  { count :: Int
  }
  deriving (Data.Data, Eq, Ord, Show)
