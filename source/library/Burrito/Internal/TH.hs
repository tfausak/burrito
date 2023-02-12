module Burrito.Internal.TH
  ( expandTH,
    uriTemplate,
  )
where

import qualified Burrito.Internal.Expand as Expand
import qualified Burrito.Internal.Parse as Parse
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Value as Value
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

-- | This can be used together with the @TemplateHaskell@ language extension to
-- expand a URI template at compile time. This is slightly different from
-- 'Expand.expand' in that missing variables will throw an exception. This is
-- convenient because it allows you to verify that all of the variables have
-- been supplied at compile time.
--
-- >>> :set -XQuasiQuotes -XTemplateHaskell
-- >>> import Burrito
-- >>> $( expandTH [("foo", stringValue "bar")] [uriTemplate|l-{foo}-r|] )
-- "l-bar-r"
expandTH :: [(String, Value.Value)] -> Template.Template -> TH.Q TH.Exp
expandTH xs t = do
  let m = Map.fromList $ fmap (Bifunctor.first Text.pack) xs
      f k =
        maybe (Left $ "missing variable: " <> show k) (Right . Just) $
          Map.lookup k m
  x <-
    either fail (pure . LazyText.unpack . Builder.toLazyText) $
      Expand.expandWith f t
  TH.lift x

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
uriTemplate =
  TH.QuasiQuoter
    { TH.quoteDec = const $ fail "cannot be used as a declaration",
      TH.quoteExp = maybe (fail "invalid URI template") TH.liftData . Parse.parse,
      TH.quotePat = const $ fail "cannot be used as a pattern",
      TH.quoteType = const $ fail "cannot be used as a type"
    }
