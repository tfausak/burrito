module Burrito.Internal.Match where

import qualified Burrito.Internal.Expand as Expand
import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Case as Case
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Value as Value
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Control.Monad as Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Text.ParserCombinators.ReadP as ReadP

match :: String -> Template.Template -> [[(String, Value.Value)]]
match s =
  fmap renderNames
    . Maybe.mapMaybe (keepConsistent . fst)
    . flip ReadP.readP_to_S s
    . template

renderNames :: [(Name.Name, Value.Value)] -> [(String, Value.Value)]
renderNames =
  fmap . Bifunctor.first $ LazyText.unpack . Builder.toLazyText . Render.name

keepConsistent
  :: [(Name.Name, Value.Value)] -> Maybe [(Name.Name, Value.Value)]
keepConsistent xs = case xs of
  [] -> Just xs
  x@(k, v) : ys -> do
    let (ts, fs) = List.partition ((== k) . fst) ys
    Monad.guard $ all ((== v) . snd) ts
    (x :) <$> keepConsistent fs

template :: Template.Template -> ReadP.ReadP [(Name.Name, Value.Value)]
template x = do
  xs <- fmap mconcat . traverse token $ Template.tokens x
  ReadP.eof
  pure xs

token :: Token.Token -> ReadP.ReadP [(Name.Name, Value.Value)]
token x = case x of
  Token.Expression y -> expression y
  Token.Literal y -> [] <$ literal y

expression :: Expression.Expression -> ReadP.ReadP [(Name.Name, Value.Value)]
expression x = variables (Expression.operator x) (Expression.variables x)

variables
  :: Operator.Operator
  -> NonEmpty.NonEmpty Variable.Variable
  -> ReadP.ReadP [(Name.Name, Value.Value)]
variables op vs = case NonEmpty.toList vs of
  [y] -> case op of
    Operator.Ampersand ->
      (ReadP.char '&'
        *> name (Variable.name y)
        *> ReadP.char '='
        *> variable Expand.isUnreserved y
        )
        ReadP.<++ pure []
    Operator.FullStop ->
      (ReadP.char '.' *> variable Expand.isUnreserved y) ReadP.<++ pure []
    Operator.None -> variable Expand.isUnreserved y
    Operator.NumberSign ->
      (ReadP.char '#' *> variable Expand.isAllowed y) ReadP.<++ pure []
    Operator.PlusSign -> variable Expand.isAllowed y
    Operator.QuestionMark ->
      (ReadP.char '?'
        *> name (Variable.name y)
        *> ReadP.char '='
        *> variable Expand.isUnreserved y
        )
        ReadP.<++ pure []
    Operator.Semicolon ->
      (ReadP.char ';'
        *> name (Variable.name y)
        *> ReadP.char '='
        *> variable Expand.isUnreserved y
        )
        ReadP.<++ (ReadP.char ';' *> name (Variable.name y) *> pure
                    [(Variable.name y, Value.String $ Text.pack "")]
                  )
        ReadP.<++ pure []
    Operator.Solidus ->
      (ReadP.char '/' *> variable Expand.isUnreserved y) ReadP.<++ pure []
  _ -> fail "TODO: multiple variables"

name :: Name.Name -> ReadP.ReadP ()
name =
  Monad.void
    . ReadP.string
    . LazyText.unpack
    . Builder.toLazyText
    . Render.name

variable
  :: (Char -> Bool)
  -> Variable.Variable
  -> ReadP.ReadP [(Name.Name, Value.Value)]
variable f x = case Variable.modifier x of
  Modifier.None -> do
    v <- Value.String <$> manyCharacters f
    pure [(Variable.name x, v)]
  _ -> fail "TODO: modifiers"

manyCharacters :: (Char -> Bool) -> ReadP.ReadP Text.Text
manyCharacters f =
  mconcat <$> many (someEncodedCharacters ReadP.<++ someUnencodedCharacters f)

many :: ReadP.ReadP a -> ReadP.ReadP [a]
many p = ((:) <$> p <*> many p) ReadP.+++ pure []

someEncodedCharacters :: ReadP.ReadP Text.Text
someEncodedCharacters =
  Text.decodeUtf8With Text.lenientDecode
    . ByteString.pack
    . fmap (uncurry Digit.toWord8)
    . NonEmpty.toList
    <$> some anEncodedCharacter

some :: ReadP.ReadP a -> ReadP.ReadP (NonEmpty.NonEmpty a)
some p = NonEmpty.NonEmpty <$> p <*> many p

someUnencodedCharacters :: (Char -> Bool) -> ReadP.ReadP Text.Text
someUnencodedCharacters f =
  Text.pack . NonEmpty.toList <$> some (ReadP.satisfy f)

anEncodedCharacter :: ReadP.ReadP (Digit.Digit, Digit.Digit)
anEncodedCharacter = do
  Monad.void $ ReadP.char '%'
  (,) <$> aDigit <*> aDigit

aDigit :: ReadP.ReadP Digit.Digit
aDigit = do
  x <- ReadP.satisfy Char.isHexDigit
  maybe (fail "invalid Digit") pure $ Digit.fromChar x

literal :: Literal.Literal -> ReadP.ReadP ()
literal = mapM_ literalCharacter . NonEmpty.toList . Literal.characters

literalCharacter :: Character.Character Literal.Literal -> ReadP.ReadP ()
literalCharacter = character Expand.isAllowed

character :: (Char -> Bool) -> Character.Character tag -> ReadP.ReadP ()
character f x = case x of
  Character.Encoded y z -> encodedCharacter y z
  Character.Unencoded y -> unencodedCharacter f y

encodedCharacter :: Digit.Digit -> Digit.Digit -> ReadP.ReadP ()
encodedCharacter x y = ReadP.char '%' *> digit x *> digit y

digit :: Digit.Digit -> ReadP.ReadP ()
digit x = Monad.void . ReadP.char $ case x of
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
unencodedCharacter f x = if f x
  then Monad.void $ ReadP.char x
  else mapM_ (uncurry encodedCharacter) $ Expand.encodeCharacter x
