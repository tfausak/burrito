-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Type.Value
  ( Value(..)
  )
where


-- | Represents a value that can be substituted into a template. Can be a
-- string, a list, or dictionary (which is called an associative array in the
-- RFC).
data Value
  = Dictionary [(String, String)]
  | List [String]
  | String String
  deriving (Eq, Show)
