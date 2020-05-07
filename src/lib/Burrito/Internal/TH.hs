module Burrito.Internal.TH where

import qualified Burrito.Internal.Parse as Parse
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

uriTemplate :: TH.QuasiQuoter
uriTemplate = TH.QuasiQuoter
  { TH.quoteDec = const $ fail "cannot be used as a declaration"
  , TH.quoteExp = maybe (fail "invalid URI template") TH.liftData . Parse.parse
  , TH.quotePat = const $ fail "cannot be used as a pattern"
  , TH.quoteType = const $ fail "cannot be used as a type"
  }
