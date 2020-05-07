module Burrito.Internal.Render where

import qualified Burrito.Internal.Type.Case as Case
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Field as Field
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder

render :: Template.Template -> String
render = LazyText.unpack . Builder.toLazyText . template

template :: Template.Template -> Builder.Builder
template = foldMap token . Template.tokens

token :: Token.Token -> Builder.Builder
token x = case x of
  Token.Expression y -> expression y
  Token.Literal y -> literal y

expression :: Expression.Expression -> Builder.Builder
expression x =
  Builder.singleton '{'
    <> operator (Expression.operator x)
    <> sepBy1 variable (Builder.singleton ',') (Expression.variables x)
    <> Builder.singleton '}'

operator :: Operator.Operator -> Builder.Builder
operator x = case x of
  Operator.Ampersand -> Builder.singleton '&'
  Operator.FullStop -> Builder.singleton '.'
  Operator.None -> mempty
  Operator.NumberSign -> Builder.singleton '#'
  Operator.PlusSign -> Builder.singleton '+'
  Operator.QuestionMark -> Builder.singleton '?'
  Operator.Semicolon -> Builder.singleton ';'
  Operator.Solidus -> Builder.singleton '/'

sepBy1
  :: (a -> Builder.Builder)
  -> Builder.Builder
  -> NonEmpty.NonEmpty a
  -> Builder.Builder
sepBy1 f x = mconcat . List.intersperse x . fmap f . NonEmpty.toList

variable :: Variable.Variable -> Builder.Builder
variable x = name (Variable.name x) <> modifier (Variable.modifier x)

name :: Name.Name -> Builder.Builder
name = sepBy1 field (Builder.singleton '.') . Name.fields

field :: Field.Field -> Builder.Builder
field = foldMap character . Field.characters

character :: Character.Character tag -> Builder.Builder
character x = case x of
  Character.Encoded y z -> encodedCharacter y z
  Character.Unencoded y -> Builder.singleton y

encodedCharacter :: Digit.Digit -> Digit.Digit -> Builder.Builder
encodedCharacter x y = Builder.singleton '%' <> digit x <> digit y

digit :: Digit.Digit -> Builder.Builder
digit x = Builder.singleton $ case x of
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

modifier :: Modifier.Modifier -> Builder.Builder
modifier x = case x of
  Modifier.Asterisk -> Builder.singleton '*'
  Modifier.Colon y -> Builder.singleton ':' <> maxLength y
  Modifier.None -> mempty

maxLength :: MaxLength.MaxLength -> Builder.Builder
maxLength = Builder.fromString . show . MaxLength.count

literal :: Literal.Literal -> Builder.Builder
literal = foldMap character . Literal.characters
