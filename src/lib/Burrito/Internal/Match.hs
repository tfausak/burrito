module Burrito.Internal.Match where

import qualified Burrito.Internal.Expand as Expand
import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Case as Case
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Value as Value
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Text.ParserCombinators.ReadP as ReadP

match :: String -> Template.Template -> [[(String, Value.Value)]]
match s t =
  Maybe.mapMaybe (keepConsistent . fst) $ ReadP.readP_to_S (template t) s

keepConsistent :: [(String, Value.Value)] -> Maybe [(String, Value.Value)]
keepConsistent xs = case xs of
  [] -> Just xs
  x@(k, v) : ys -> do
    let (ts, fs) = List.partition ((== k) . fst) ys
    Monad.guard $ all ((== v) . snd) ts
    (x :) <$> keepConsistent fs

type Matcher = ReadP.ReadP [(String, Value.Value)]

template :: Template.Template -> Matcher
template x = do
  xs <- fmap mconcat . traverse token $ Template.tokens x
  ReadP.eof
  pure xs

token :: Token.Token -> Matcher
token x = case x of
  Token.Expression y -> expression y
  Token.Literal y -> literal y

expression :: Expression.Expression -> Matcher
expression x =
  let
    o = Expression.operator x
    f =
      fmap mconcat
        . traverse (variable o)
        . NonEmpty.toList
        $ Expression.variables x
  in case o of
    Operator.None -> f
    _ -> ReadP.pfail

variable :: Operator.Operator -> Variable.Variable -> Matcher
variable o v = do
  let
    many p = ((:) <$> p <*> many p) ReadP.+++ pure []
    f p = Value.String . Text.pack <$> many (ReadP.satisfy p)
  x <- case o of
    Operator.None -> f Expand.isUnreserved
    _ -> ReadP.pfail
  pure
    [(LazyText.unpack . Builder.toLazyText . Render.name $ Variable.name v, x)]

literal :: Literal.Literal -> Matcher
literal =
  fmap mconcat
    . traverse literalCharacter
    . NonEmpty.toList
    . Literal.characters

literalCharacter :: Character.Character Literal.Literal -> Matcher
literalCharacter x = case x of
  Character.Encoded y z -> encodedCharacter y z
  Character.Unencoded y -> unencodedCharacter y

encodedCharacter :: Digit.Digit -> Digit.Digit -> Matcher
encodedCharacter x y = ReadP.char '%' *> digit x *> digit y

digit :: Digit.Digit -> Matcher
digit x = do
  Monad.void . ReadP.char $ case x of
    Digit.Ox0 -> '0'
    Digit.Ox1 -> '1'
    Digit.Ox2 -> '2'
    Digit.Ox3 -> '3'
    Digit.Ox4 -> '4'
    Digit.Ox5 -> '5'
    Digit.Ox6 -> '6'
    Digit.Ox7 -> '7'
    Digit.Ox8 -> '8'
    Digit.Ox9 -> '9'
    Digit.OxA Case.Upper -> 'A'
    Digit.OxB Case.Upper -> 'B'
    Digit.OxC Case.Upper -> 'C'
    Digit.OxD Case.Upper -> 'D'
    Digit.OxE Case.Upper -> 'E'
    Digit.OxF Case.Upper -> 'F'
    Digit.OxA Case.Lower -> 'a'
    Digit.OxB Case.Lower -> 'b'
    Digit.OxC Case.Lower -> 'c'
    Digit.OxD Case.Lower -> 'd'
    Digit.OxE Case.Lower -> 'e'
    Digit.OxF Case.Lower -> 'f'
  pure []

unencodedCharacter :: Char -> Matcher
unencodedCharacter x =
  ([] <$ ReadP.char x)
    ReadP.<++ (fmap mconcat
              . traverse (uncurry encodedCharacter)
              $ Expand.encodeCharacter x
              )
