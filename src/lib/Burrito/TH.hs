-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.TH
  ( uriTemplate
  )
where

import qualified Burrito.Parse as Parse
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH


-- | This can be used together with the @QuasiQuotes@ language extension to
-- parse a URI template at compile time. This is convenient because it allows
-- you to verify the validity of the template when you compile your file as
-- opposed to when you run it.
--
-- >>> :set -XQuasiQuotes
-- >>> import Burrito
-- >>> let template = [uriTemplate|http://example/search{?query}|]
-- >>> let values = [("query", stringValue "chorizo")]
-- >>> expand values template
-- "http://example/search?query=chorizo"
--
-- Note that you cannot use escape sequences in this quasi-quoter. For example,
-- this is invalid: @[uriTemplate|\\xa0|]@. You can however use percent encoded
-- triples as normal. So this is valid: @[uriTemplate|%c2%a0|]@.
uriTemplate :: TH.QuasiQuoter
uriTemplate = TH.QuasiQuoter
  { TH.quoteDec = const $ fail "cannot be used as a declaration"
  , TH.quoteExp = maybe (fail "invalid URI template") TH.lift . Parse.parse
  , TH.quotePat = const $ fail "cannot be used as a pattern"
  , TH.quoteType = const $ fail "cannot be used as a type"
  }
