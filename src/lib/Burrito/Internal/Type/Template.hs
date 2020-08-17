{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Template (Template (..), render) where

import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Token as Token
import qualified Data.Data as Data
import qualified Data.Text.Lazy.Builder as Builder

-- | Represents a URI template.
newtype Template = Template
  { tokens :: [Token.Token]
  } deriving (Data.Data, Eq, Ord)

instance Show Template where
  show = render

-- | Renders a template back into a string. This is essentially the opposite of
-- @parse@. Usually you'll want to use @expand@ to actually substitute
-- variables in the template, but this can be useful for printing out the
-- template itself
--
-- >>> render <$> parse "valid-template"
-- Just "valid-template"
-- >>> render <$> parse "{var}"
-- Just "{var}"
render :: Template -> String
render = Render.builderToString . template

template :: Template -> Builder.Builder
template = foldMap Render.token . tokens
