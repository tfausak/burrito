{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Burrito.Internal.Match where

import qualified Burrito.Internal.Expand as Expand
import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Case as Case
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.Match as Match
import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Value as Value
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.ParserCombinators.ReadP as ReadP

-- | Matches a string against a template. This is essentially the opposite of
-- @expand@.
--
-- Since there isn't always one unique match, this function returns all the
-- possibilities. It's up to you to select the one that makes the most sense,
-- or to simply grab the first one if you don't care.
--
-- >>> match "" <$> parse "no-match"
-- Just []
-- >>> match "no-variables" <$> parse "no-variables"
-- Just [[]]
-- >>> match "1-match" <$> parse "{one}-match"
-- Just [[("one",String "1")]]
--
-- Be warned that the number of possible matches can grow quickly if your
-- template has variables next to each other without any separators.
--
-- >>> let Just template = parse "{a}{b}"
-- >>> mapM_ print $ match "ab" template
-- [("a",String "a"),("b",String "b")]
-- [("a",String "ab"),("b",String "")]
-- [("a",String "ab")]
-- [("a",String ""),("b",String "ab")]
-- [("b",String "ab")]
--
-- Matching supports everything /except/ explode modifiers (@{a*}@), list
-- values, and dictionary values.
match :: String -> Template.Template -> [[(String, Value.Value)]]
match s =
  fmap finalize
    . Maybe.mapMaybe (keepConsistent . fst)
    . flip ReadP.readP_to_S s
    . template

finalize :: [(Name.Name, Match.Match)] -> [(String, Value.Value)]
finalize = Maybe.mapMaybe $ \(n, m) -> case m of
  Match.Defined v ->
    Just (Render.builderToString $ Render.name n, Value.String v)
  Match.Prefix _ v ->
    Just (Render.builderToString $ Render.name n, Value.String v)
  Match.Undefined -> Nothing

keepConsistent ::
  [(Name.Name, Match.Match)] -> Maybe [(Name.Name, Match.Match)]
keepConsistent xs = case xs of
  [] -> Just xs
  (k, v) : ys -> do
    let (ts, fs) = List.partition ((== k) . fst) ys
    w <- combine v $ fmap snd ts
    ((k, w) :) <$> keepConsistent fs

combine :: Match.Match -> [Match.Match] -> Maybe Match.Match
combine x ys = case ys of
  [] -> Just x
  y : zs -> case x of
    Match.Defined t -> case y of
      Match.Defined u | t == u -> combine x zs
      Match.Prefix m u | Text.take (MaxLength.count m) t == u -> combine x zs
      _ -> Nothing
    Match.Prefix n t -> case y of
      Match.Defined u | t == Text.take (MaxLength.count n) u -> combine y zs
      Match.Prefix m u
        | let c = MaxLength.count (min n m) in Text.take c t == Text.take c u ->
            combine (if m > n then y else x) zs
      _ -> Nothing
    Match.Undefined -> case y of
      Match.Undefined -> combine x zs
      _ -> Nothing

template :: Template.Template -> ReadP.ReadP [(Name.Name, Match.Match)]
template x = do
  xs <- fmap mconcat . traverse token $ Template.tokens x
  ReadP.eof
  pure xs

token :: Token.Token -> ReadP.ReadP [(Name.Name, Match.Match)]
token x = case x of
  Token.Expression y -> expression y
  Token.Literal y -> [] <$ literal y

expression :: Expression.Expression -> ReadP.ReadP [(Name.Name, Match.Match)]
expression x = variables (Expression.operator x) (Expression.variables x)

variables ::
  Operator.Operator ->
  NonEmpty.NonEmpty Variable.Variable ->
  ReadP.ReadP [(Name.Name, Match.Match)]
variables op vs = case op of
  Operator.Ampersand -> vars vs (Just '&') '&' varEq
  Operator.FullStop -> vars vs (Just '.') '.' $ variable Expand.isUnreserved
  Operator.None -> vars vs Nothing ',' $ variable Expand.isUnreserved
  Operator.NumberSign -> vars vs (Just '#') ',' $ variable Expand.isAllowed
  Operator.PlusSign -> vars vs Nothing ',' $ variable Expand.isAllowed
  Operator.QuestionMark -> vars vs (Just '?') '&' varEq
  Operator.Semicolon -> vars vs (Just ';') ';' $ \v -> do
    let n = Variable.name v
    name n
    ReadP.option [(n, Match.Defined Text.empty)] $ do
      char_ '='
      variable Expand.isUnreserved v
  Operator.Solidus -> vars vs (Just '/') '/' $ variable Expand.isUnreserved

vars ::
  NonEmpty.NonEmpty Variable.Variable ->
  Maybe Char ->
  Char ->
  (Variable.Variable -> ReadP.ReadP [(Name.Name, Match.Match)]) ->
  ReadP.ReadP [(Name.Name, Match.Match)]
vars vs m c f = do
  let ctx = case m of
        Nothing -> id
        Just o -> \p -> ReadP.option (undef <$> NonEmpty.toList vs) $ do
          char_ o
          xs <- p
          Monad.guard . not $ all isUndefined xs
          pure xs
  ctx . vars' c f $ NonEmpty.toList vs

isUndefined :: (Name.Name, Match.Match) -> Bool
isUndefined = (== Match.Undefined) . snd

vars' ::
  Char ->
  (Variable.Variable -> ReadP.ReadP [(Name.Name, Match.Match)]) ->
  [Variable.Variable] ->
  ReadP.ReadP [(Name.Name, Match.Match)]
vars' c f vs = case vs of
  [] -> pure []
  v : ws ->
    let this = do
          x <- f v
          xs <- ReadP.option (undef <$> ws) $ do
            char_ c
            vars' c f ws
          pure $ x <> xs
        that = (undef v :) <$> vars' c f ws
     in this ReadP.+++ that

undef :: Variable.Variable -> (Name.Name, Match.Match)
undef v = (Variable.name v, Match.Undefined)

char_ :: Char -> ReadP.ReadP ()
char_ = Monad.void . ReadP.char

varEq :: Variable.Variable -> ReadP.ReadP [(Name.Name, Match.Match)]
varEq v = do
  name $ Variable.name v
  char_ '='
  variable Expand.isUnreserved v

name :: Name.Name -> ReadP.ReadP ()
name = Monad.void . ReadP.string . Render.builderToString . Render.name

variable ::
  (Char -> Bool) ->
  Variable.Variable ->
  ReadP.ReadP [(Name.Name, Match.Match)]
variable f x = do
  v <- case Variable.modifier x of
    Modifier.Asterisk -> ReadP.pfail
    Modifier.None -> Match.Defined <$> manyCharacters f
    Modifier.Colon n -> Match.Prefix n <$> manyCharacters f
  pure [(Variable.name x, v)]

manyCharacters :: (Char -> Bool) -> ReadP.ReadP Text.Text
manyCharacters f = do
  let f1 = (:) <$> someEncodedCharacters <*> ReadP.option [] f2
      f2 = (:) <$> someUnencodedCharacters f <*> ReadP.option [] f1
  fmap mconcat . ReadP.option [] $ f1 ReadP.<++ f2

someEncodedCharacters :: ReadP.ReadP Text.Text
someEncodedCharacters = do
  xs <- some anEncodedCharacter
  either (fail . show) pure
    . Text.decodeUtf8'
    . ByteString.pack
    . fmap (uncurry Digit.toWord8)
    $ NonEmpty.toList xs

some :: ReadP.ReadP a -> ReadP.ReadP (NonEmpty.NonEmpty a)
some p = (NonEmpty.:|) <$> p <*> ReadP.many p

someUnencodedCharacters :: (Char -> Bool) -> ReadP.ReadP Text.Text
someUnencodedCharacters f = do
  xs <- some $ ReadP.satisfy f
  pure . Text.pack $ NonEmpty.toList xs

anEncodedCharacter :: ReadP.ReadP (Digit.Digit, Digit.Digit)
anEncodedCharacter = do
  char_ '%'
  (,) <$> aDigit <*> aDigit

aDigit :: ReadP.ReadP Digit.Digit
aDigit = do
  x <- ReadP.satisfy Char.isHexDigit
  maybe (fail "invalid Digit") pure $ Digit.fromChar x

literal :: Literal.Literal -> ReadP.ReadP ()
literal = mapM_ literalCharacter . Literal.characters

literalCharacter :: Character.Character Literal.Literal -> ReadP.ReadP ()
literalCharacter = character Expand.isAllowed

character :: (Char -> Bool) -> Character.Character tag -> ReadP.ReadP ()
character f x = case x of
  Character.Encoded y z -> encodedCharacter y z
  Character.Unencoded y -> unencodedCharacter f y

encodedCharacter :: Digit.Digit -> Digit.Digit -> ReadP.ReadP ()
encodedCharacter x y = char_ '%' *> digit x *> digit y

digit :: Digit.Digit -> ReadP.ReadP ()
digit x = char_ $ case x of
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

unencodedCharacter :: (Char -> Bool) -> Char -> ReadP.ReadP ()
unencodedCharacter f x =
  if f x
    then char_ x
    else mapM_ (uncurry encodedCharacter) $ Expand.encodeCharacter x
