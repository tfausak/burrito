{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Modifier
  ( Modifier(..)
  , makeColon
  )
where

import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a modifier on a variable.
data Modifier
  = Asterisk
  | Colon Int
  -- ^ This assumes that the number passes the @isValidMaxLength@ predicate.
  -- You should prefer using @makeColon@ to create these values.
  | None
  deriving (Eq, TH.Lift, Show)


-- | If the number passes @isValidMaxLength@, returns a @Colon@ modifier.
-- Otherwise returns nothing.
makeColon :: Int -> Maybe Modifier
makeColon maxLength =
  if isValidMaxLength maxLength then Just $ Colon maxLength else Nothing


-- | Returns true if the given number is a valid @max-length@ as defined by
-- section 2.4.1 of the RFC.
isValidMaxLength :: Int -> Bool
isValidMaxLength maxLength = 1 <= maxLength && maxLength <= 9999
