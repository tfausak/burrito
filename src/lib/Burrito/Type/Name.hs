{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Name
  ( Name(..)
  )
where

import qualified Burrito.Type.VarChar as VarChar
import qualified Language.Haskell.TH.Syntax as TH


-- | Represents a variable name, which is required to be non-empty. Variable
-- names allow ASCII letters and numbers, underscores, percent encoded triples,
-- and periods. However the periods cannot appear at the beginning or end, and
-- there can't be more than one of them in a row.
data Name = Name
  { first :: VarChar.VarChar
  -- ^ The first character is any valid @varchar@, including underscores and
  -- percent encoded triples.
  , rest :: [(Bool, VarChar.VarChar)]
  -- ^ Every other character has the same constraints, but they may also be
  -- preceeded by a full stop (period). That's what the @Bool@ represents:
  -- @True@ means there is a period.
  } deriving (Eq, TH.Lift, Show)
