{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Operator where

import qualified Data.Data as Data

data Operator
  = Ampersand
  | FullStop
  | None
  | NumberSign
  | PlusSign
  | QuestionMark
  | Semicolon
  | Solidus
  deriving (Data.Data, Eq, Show)
