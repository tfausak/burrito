module Burrito.TH ( templateExpression, templatePattern ) where

import qualified Burrito

import Language.Haskell.TH.Syntax (Exp, Pat, Q, lift)

templateExpression :: Burrito.Template -> Q Exp
templateExpression = lift

templatePattern :: Burrito.Template -> Q Pat
templatePattern _ = fail "Template pattern: not yet implemented"
