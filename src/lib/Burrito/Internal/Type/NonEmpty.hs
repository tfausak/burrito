{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.NonEmpty where

import qualified Data.Data as Data

data NonEmpty a = NonEmpty
  { head :: a
  , tail :: [a]
  } deriving (Data.Data, Eq, Show)

toList :: NonEmpty a -> [a]
toList (NonEmpty h t) = h : t
