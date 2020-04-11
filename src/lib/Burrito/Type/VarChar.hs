{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.VarChar
  ( VarChar(..)
  , makeEncoded
  , makeUnencoded
  , isVarchar
  )
where

import qualified Data.Char as Char
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a single logical character in a variable name.
data VarChar
  = Encoded Char Char
  -- ^ A percent encoded triple. Note that this represents three literal
  -- characters in the input, even though logically we always treat it as one
  -- character. The two arguments are the high and the low hexadecimal digits,
  -- respectively. This representation intentially keeps track of their case,
  -- so as to avoid confusing values like @%aa@ and @%AA@. You should use
  -- @makeEncoded@ to build these values.
  | Unencoded Char
  -- ^ A literal unencoded character. You should use @makeUnencoded@ to build
  -- these values.
  deriving (Eq, TH.Lift, Show)


-- | Makes sure that both characters are valid hexadecimal digits. If they are,
-- returns an @Encoded@ character. Otherwise returns nothing.
makeEncoded :: Char -> Char -> Maybe VarChar
makeEncoded hi lo =
  if Char.isHexDigit hi && Char.isHexDigit lo
  then Just $ Encoded hi lo
  else Nothing


-- | Makes sure that the character passes the @isVarchar@ predicate. If it
-- does, returns an @Unencoded@ character. Otherwise returns nothing.
makeUnencoded :: Char -> Maybe VarChar
makeUnencoded char = if isVarchar char then Just $ Unencoded char else Nothing


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
