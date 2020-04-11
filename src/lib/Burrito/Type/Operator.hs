{-# LANGUAGE DeriveLift #-}

-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Operator
  ( Operator(..)
  )
where

import qualified Language.Haskell.TH.Syntax as TH


-- | Represents an operator in an expression.
data Operator
  = Ampersand
  | FullStop
  | None
  | NumberSign
  | PlusSign
  | QuestionMark
  | Semicolon
  | Solidus
  deriving (Eq, TH.Lift, Show)
