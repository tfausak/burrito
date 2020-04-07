module Burrito.Type.Name
  ( Name(..)
  ) where

import qualified Data.List.NonEmpty as NonEmpty


-- | Represents a variable name, which is required to be non-empty. Variable
-- names allow ASCII letters and numbers, underscores, percent encoded triples,
-- and periods. However the periods cannot appear at the beginning or end, and
-- there can't be more than one of them in a row.
newtype Name = Name
  { chars :: NonEmpty.NonEmpty Char
  } deriving (Eq, Show)
