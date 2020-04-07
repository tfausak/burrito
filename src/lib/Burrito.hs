-- | Burrito is a Haskell library for parsing and rendering URI templates.
--
-- According to [RFC 6570](https://tools.ietf.org/html/rfc6570): "A URI
-- Template is a compact sequence of characters for describing a range of
-- Uniform Resource Identifiers through variable expansion." Burrito implements
-- URI templates according to the specification in that RFC.
--
-- The term "uniform resource identifiers" (URI) is often used interchangeably
-- with other related terms like "internationalized resource identifier" (IRI),
-- "uniform resource locator" (URL), and "uniform resource name" (URN). Burrito
-- can be used for all of these. If you want to get technical, its input must
-- be a valid IRI and its output will be a valid URI or URN.
--
-- Although Burrito is primarily intended to be used with HTTP and HTTPS URIs,
-- it should work with other schemes as well.
--
-- If you're not already familiar with URI templates, I recommend reading the
-- overview of the RFC. It's short, to the point, and easy to understand.
--
-- Assuming you're familiar with URI templates, here's a simple example to show
-- you how Burrito works:
--
-- > import Burrito
-- > let Just template = parse "http://example.com/search{?query}"
-- > expand [ ( "query", stringValue "bikes" ) ] template
-- > "http://example.com/search?query=bikes"
--
-- In short, use 'parse' to parse templates and 'expand' to render them.
module Burrito
  ( Parse.parse
  , Expand.expand
  , Template.Template
  , Value.Value
  , stringValue
  , listValue
  , dictionaryValue
  )
where

import qualified Burrito.Expand as Expand
import qualified Burrito.Parse as Parse
import qualified Burrito.Type.Template as Template
import qualified Burrito.Type.Value as Value


-- | Constructs a string value.
stringValue :: String -> Value.Value
stringValue = Value.String


-- | Constructs a list value.
listValue :: [String] -> Value.Value
listValue = Value.List


-- | Constructs a dictionary value.
dictionaryValue :: [(String, String)] -> Value.Value
dictionaryValue = Value.Dictionary
