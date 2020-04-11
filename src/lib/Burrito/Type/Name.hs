{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Name
  ( Name(..)
  , isVarchar
  ) where

import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Data.Char as Char
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a variable name, which is required to be non-empty. Variable
-- names allow ASCII letters and numbers, underscores, percent encoded triples,
-- and periods. However the periods cannot appear at the beginning or end, and
-- there can't be more than one of them in a row.
newtype Name = Name
  { chars :: NonEmpty.NonEmpty Char
  } deriving (Eq, TH.Lift, Show)


-- | Returns true if the given character is in the @varchar@ range defined by
-- section 2.3 of the RFC. Note that this does not include the @pct-encoded@
-- part of the grammar because that requires multiple characters to match.
isVarchar :: Char -> Bool
isVarchar x = case x of
  '_' -> True
  _ -> isAlpha x || Char.isDigit x


-- | Returns true if the given character is in the @ALPHA@ range defined by
-- section 1.5 of the RFC.
isAlpha :: Char -> Bool
isAlpha x = Char.isAsciiUpper x || Char.isAsciiLower x
