module Burrito.QQ ( uriTemplate ) where

import qualified Burrito
import qualified Burrito.TH
import qualified Language.Haskell.TH.Quote as Q

import Language.Haskell.TH.Syntax (Q)

uriTemplate :: Q.QuasiQuoter
uriTemplate = Q.QuasiQuoter
  { Q.quoteExp  = maybe invalid Burrito.TH.templateExpression . Burrito.parse
  , Q.quotePat  = maybe invalid Burrito.TH.templatePattern . Burrito.parse
  , Q.quoteType = \_ -> fail "uriTemplate cannot be used in a type context"
  , Q.quoteDec  = \_ -> fail "uriTemplate cannot be used in a declaration context"
  }
  where
    invalid :: Q a
    invalid = fail "Invalid URI template expression"
