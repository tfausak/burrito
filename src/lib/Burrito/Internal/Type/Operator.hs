{-# LANGUAGE DeriveLift #-}

module Burrito.Internal.Type.Operator where

import qualified Language.Haskell.TH.Syntax as TH

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
