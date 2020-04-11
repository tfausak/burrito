{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!
module Burrito.Type.LitChar
  ( LitChar(..)
  , isLiteral
  , makeUnencoded
  ) where

import qualified Data.Word as Word
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a character in a literal. Although encoded characters are
-- allowed to have any value, typically they will not include most ASCII
-- printable characters. In other words @A@ is more likely than @%41@.
data LitChar
  = Encoded Word.Word8
  -- ^ This deliberately is not case sensitive. The tokens @%aa@ and @%AA@ are
  -- both represented as @Encoded 0xAA@.
  | Unencoded Char
  -- ^ This assumes that the character passes the @isLiteral@ predicate. You
  -- should prefer using @makeUnencoded@ to create these values.
  deriving (Eq, TH.Lift, Show)


-- | If the character passes @isLiteral@, returns an @Unencoded@ character.
-- Otherwise returns nothing.
makeUnencoded :: Char -> Maybe LitChar
makeUnencoded char = if isLiteral char then Just $ Unencoded char else Nothing


-- | Returns true if the given character is in the @literal@ range defined by
-- section 2.1 of the RFC.
isLiteral :: Char -> Bool
isLiteral x = case x of
  ' ' -> False
  '"' -> False
  '\'' -> False
  '%' -> False
  '<' -> False
  '>' -> False
  '\\' -> False
  '^' -> False
  '`' -> False
  '{' -> False
  '|' -> False
  '}' -> False
  _ -> between '\x20' '\x7e' x || isUcschar x || isIprivate x


-- | Returns true if the given character is in the @ucschar@ range defined by
-- section 1.5 of the RFC.
isUcschar :: Char -> Bool
isUcschar x =
  between '\xa0' '\xd7ff' x
    || between '\xf900' '\xfdcf' x
    || between '\xfdf0' '\xffef' x
    || between '\x10000' '\x1fffd' x
    || between '\x20000' '\x2fffd' x
    || between '\x30000' '\x3fffd' x
    || between '\x40000' '\x4fffd' x
    || between '\x50000' '\x5fffd' x
    || between '\x60000' '\x6fffd' x
    || between '\x70000' '\x7fffd' x
    || between '\x80000' '\x8fffd' x
    || between '\x90000' '\x9fffd' x
    || between '\xa0000' '\xafffd' x
    || between '\xb0000' '\xbfffd' x
    || between '\xc0000' '\xcfffd' x
    || between '\xd0000' '\xdfffd' x
    || between '\xe1000' '\xefffd' x


-- | Returns true if the given character is in the @iprivate@ range defined by
-- section 1.5 of the RFC.
isIprivate :: Char -> Bool
isIprivate x =
  between '\xe000' '\xf8ff' x
    || between '\xf0000' '\xffffd' x
    || between '\x100000' '\x10fffd' x


-- | Returns true if the value is between the given inclusive bounds.
between
  :: Ord a
  => a -- ^ lower bound
  -> a -- ^ upper bound
  -> a
  -> Bool
between lo hi x = lo <= x && x <= hi
