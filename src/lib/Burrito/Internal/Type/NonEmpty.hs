{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.NonEmpty where

import qualified Language.Haskell.TH.Syntax as TH

data NonEmpty a = NonEmpty
  { head :: a
  , tail :: [a]
  } deriving (Eq, TH.Lift, Show)

toList :: NonEmpty a -> [a]
toList (NonEmpty h t) = h : t
