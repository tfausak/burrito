{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.NonEmpty
  ( NonEmpty(..)
  , fromList
  , singleton
  , toList
  )
where

import qualified Language.Haskell.TH.Syntax as TH


-- | This simple type is used as a replacement for
-- @Data.List.NonEmpty.NonEmpty@ so that we can provide our own instances that
-- are consistent across versions of GHC.
data NonEmpty a = NonEmpty
  { first :: a
  , rest :: [a]
  } deriving (Eq, TH.Lift, Show)


-- | Attempts to convert a regular list into a non-empty list.
fromList :: [a] -> Maybe (NonEmpty a)
fromList xs = case xs of
  [] -> Nothing
  x : ys -> Just NonEmpty { first = x, rest = ys }


-- | Creates a non-empty list with a single element.
singleton :: a -> NonEmpty a
singleton x = NonEmpty { first = x, rest = [] }


-- | Converts a non-empty list back into a regular list.
toList :: NonEmpty a -> [a]
toList xs = first xs : rest xs
