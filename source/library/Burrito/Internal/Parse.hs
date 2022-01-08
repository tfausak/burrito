{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{-# LANGUAGE FlexibleContexts #-}

module Burrito.Internal.Parse where

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
import qualified Data.Char as Char
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as Parsec
import qualified Text.Read as Read

-- | Attempts to parse a string as a URI template. If parsing fails, this will
-- return @Nothing@. Otherwise it will return @Just@ the parsed template.
--
-- Parsing will usually succeed, but it can fail if the input string contains
-- characters that are not valid in IRIs (like @^@) or if the input string
-- contains an invalid template expression (like @{!}@). To include characters
-- that aren't valid in IRIs, percent encode them (like @%5E@).
--
-- >>> parse "invalid template"
-- Nothing
-- >>> parse "valid-template"
-- Just (Template ...)
parse :: String -> Maybe Template.Template
parse = either (const Nothing) Just . Parsec.parse template ""

template :: Parsec.Stream s m Char => Parsec.ParsecT s u m Template.Template
template = Template.Template <$> Parsec.many token <* Parsec.eof

token :: Parsec.Stream s m Char => Parsec.ParsecT s u m Token.Token
token = choice (Token.Expression <$> expression) (Token.Literal <$> literal)

choice
  :: Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a -> Parsec.ParsecT s u m a
choice = (Parsec.<|>)

expression
  :: Parsec.Stream s m Char => Parsec.ParsecT s u m Expression.Expression
expression =
  Parsec.between (Parsec.char '{') (Parsec.char '}')
    $ Expression.Expression
    <$> operator
    <*> sepBy1 variable (Parsec.char ',')

operator :: Parsec.Stream s m Char => Parsec.ParsecT s u m Operator.Operator
operator = Parsec.option Operator.None $ Parsec.choice
  [ Operator.Ampersand <$ Parsec.char '&'
  , Operator.FullStop <$ Parsec.char '.'
  , Operator.NumberSign <$ Parsec.char '#'
  , Operator.PlusSign <$ Parsec.char '+'
  , Operator.QuestionMark <$ Parsec.char '?'
  , Operator.Semicolon <$ Parsec.char ';'
  , Operator.Solidus <$ Parsec.char '/'
  ]

sepBy1
  :: Parsec.ParsecT s u m a
  -> Parsec.ParsecT s u m x
  -> Parsec.ParsecT s u m (NonEmpty.NonEmpty a)
sepBy1 p s = (NonEmpty.:|) <$> p <*> Parsec.many (s *> p)

variable :: Parsec.Stream s m Char => Parsec.ParsecT s u m Variable.Variable
variable = Variable.Variable <$> name <*> modifier

name :: Parsec.Stream s m Char => Parsec.ParsecT s u m Name.Name
name = Name.Name <$> sepBy1 field (Parsec.char '.')

field :: Parsec.Stream s m Char => Parsec.ParsecT s u m Field.Field
field = Field.Field <$> nonEmpty fieldCharacter

nonEmpty
  :: Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (NonEmpty.NonEmpty a)
nonEmpty p = (NonEmpty.:|) <$> p <*> Parsec.many p

fieldCharacter
  :: Parsec.Stream s m Char
  => Parsec.ParsecT s u m (Character.Character Field.Field)
fieldCharacter = choice encodedCharacter (unencodedCharacter isFieldCharacter)

encodedCharacter
  :: Parsec.Stream s m Char => Parsec.ParsecT s u m (Character.Character tag)
encodedCharacter = Parsec.char '%' >> Character.Encoded <$> digit <*> digit

digit :: Parsec.Stream s m Char => Parsec.ParsecT s u m Digit.Digit
digit = do
  x <- Parsec.satisfy Char.isHexDigit
  maybe (fail "invalid Digit") pure $ Digit.fromChar x

unencodedCharacter
  :: Parsec.Stream s m Char
  => (Char -> Bool)
  -> Parsec.ParsecT s u m (Character.Character tag)
unencodedCharacter = fmap Character.Unencoded . Parsec.satisfy

isFieldCharacter :: Char -> Bool
isFieldCharacter x = case x of
  '_' -> True
  _ -> Char.isAsciiUpper x || Char.isAsciiLower x || Char.isDigit x

modifier :: Parsec.Stream s m Char => Parsec.ParsecT s u m Modifier.Modifier
modifier = Parsec.option Modifier.None $ Parsec.choice
  [ Modifier.Asterisk <$ Parsec.char '*'
  , Parsec.char ':' >> Modifier.Colon <$> maxLength
  ]

maxLength
  :: Parsec.Stream s m Char => Parsec.ParsecT s u m MaxLength.MaxLength
maxLength = do
  x <- Parsec.satisfy $ Ix.inRange ('1', '9')
  xs <- Parsec.many $ Parsec.satisfy Char.isDigit
  n <- maybe (fail "invalid MaxLength") pure . Read.readMaybe $ x : xs
  if isMaxLength n
    then pure $ MaxLength.MaxLength n
    else fail "invalid MaxLength"

isMaxLength :: Int -> Bool
isMaxLength = Ix.inRange (1, 9999)

literal :: Parsec.Stream s m Char => Parsec.ParsecT s u m Literal.Literal
literal = Literal.Literal <$> nonEmpty literalCharacter

literalCharacter
  :: Parsec.Stream s m Char
  => Parsec.ParsecT s u m (Character.Character Literal.Literal)
literalCharacter =
  choice encodedCharacter (unencodedCharacter isLiteralCharacter)

isLiteralCharacter :: Char -> Bool
isLiteralCharacter x = case x of
  ' ' -> False
  '"' -> False
  '\'' -> False
  '%' -> False
  '<' -> False
  '>' -> False
  '\\' -> False
  '^' -> False
  '`' -> False
  '{' -> False
  '|' -> False
  '}' -> False
  _ ->
    Ix.inRange ('\x20', '\x7e') x
      || Ix.inRange ('\xa0', '\xd7ff') x
      || Ix.inRange ('\xe000', '\xf8ff') x
      || Ix.inRange ('\xf900', '\xfdcf') x
      || Ix.inRange ('\xfdf0', '\xffef') x
      || Ix.inRange ('\x10000', '\x1fffd') x
      || Ix.inRange ('\x20000', '\x2fffd') x
      || Ix.inRange ('\x30000', '\x3fffd') x
      || Ix.inRange ('\x40000', '\x4fffd') x
      || Ix.inRange ('\x50000', '\x5fffd') x
      || Ix.inRange ('\x60000', '\x6fffd') x
      || Ix.inRange ('\x70000', '\x7fffd') x
      || Ix.inRange ('\x80000', '\x8fffd') x
      || Ix.inRange ('\x90000', '\x9fffd') x
      || Ix.inRange ('\xa0000', '\xafffd') x
      || Ix.inRange ('\xb0000', '\xbfffd') x
      || Ix.inRange ('\xc0000', '\xcfffd') x
      || Ix.inRange ('\xd0000', '\xdfffd') x
      || Ix.inRange ('\xe1000', '\xefffd') x
      || Ix.inRange ('\xf0000', '\xffffd') x
      || Ix.inRange ('\x100000', '\x10fffd') x
