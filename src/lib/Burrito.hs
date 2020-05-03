module Burrito
  ( Template.Template
  , Value.Value
  , Parse.parse
  , TH.uriTemplate
  , Render.render
  , Expand.expand
  , Expand.expandWith
  , Match.match
  , stringValue
  , listValue
  , dictionaryValue
  )
where

import qualified Burrito.Internal.Expand as Expand
import qualified Burrito.Internal.Match as Match
import qualified Burrito.Internal.Parse as Parse
import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.TH as TH
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Value as Value
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text

stringValue :: String -> Value.Value
stringValue = Value.String . Text.pack

listValue :: [String] -> Value.Value
listValue = Value.List . fmap Text.pack

dictionaryValue :: [(String, String)] -> Value.Value
dictionaryValue =
  Value.Dictionary . Map.fromList . fmap (Bifunctor.bimap Text.pack Text.pack)
