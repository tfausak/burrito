-- | Warning: This module is not considered part of Burrito's public API. As
-- such, it may change at any time. Use it with caution!.
module Burrito.Expand
  ( expand
  ) where

import qualified Burrito.Type.Character as Character
import qualified Burrito.Type.Expression as Expression
import qualified Burrito.Type.Literal as Literal
import qualified Burrito.Type.Modifier as Modifier
import qualified Burrito.Type.Name as Name
import qualified Burrito.Type.NonEmpty as NonEmpty
import qualified Burrito.Type.Operator as Operator
import qualified Burrito.Type.Template as Template
import qualified Burrito.Type.Token as Token
import qualified Burrito.Type.Value as Value
import qualified Burrito.Type.Variable as Variable
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Text.Printf as Printf


-- | Expands a template using the given values. Unlike parsing, expansion
-- always succeeds. If no value is given for a variable, it will simply not
-- appear in the output.
expand :: [(String, Value.Value)] -> Template.Template -> String
expand values = Identity.runIdentity
  . expandTemplate (pure . flip lookup values . nameToString)


-- | Expands a template for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandTemplate
  :: Applicative m => (Name.Name -> m (Maybe Value.Value)) -> Template.Template -> m String
expandTemplate f = expandTokens f . Template.tokens


-- | Expands tokens for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandTokens
  :: Applicative m => (Name.Name -> m (Maybe Value.Value)) -> [Token.Token] -> m String
expandTokens f = fmap concat . traverse (expandToken f)


-- | Expands a token for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandToken :: Applicative m => (Name.Name -> m (Maybe Value.Value)) -> Token.Token -> m String
expandToken f token = case token of
  Token.Literal literal -> pure $ expandLiteral literal
  Token.Expression expression -> expandExpression f expression


-- | Expands a literal token for output according to section 3.1 of the RFC.
expandLiteral :: Literal.Literal -> String
expandLiteral = concatMap expandCharacter . NonEmpty.toList . Literal.characters


-- | Expands a single literal character for output. This is necessary to
-- normalize percent encodings and to encode characters that aren't allowed to
-- appear in URIs.
expandCharacter :: Character.Character -> String
expandCharacter character = case character of
  Character.Encoded word8 -> percentEncodeWord8 word8
  Character.Unencoded char -> escapeChar Operator.PlusSign char


-- | If necessary, escapes a character for output with the given operator.
-- Otherwise returns the character unchanged as a string.
escapeChar :: Operator.Operator -> Char -> String
escapeChar operator char =
  if isAllowed operator char then [char] else percentEncodeChar char


-- | Returns true if the given character is allowed unescaped in the output for
-- the given operator.
isAllowed :: Operator.Operator -> Char -> Bool
isAllowed operator char = case operator of
  Operator.NumberSign -> isUnreserved char || isReserved char
  Operator.PlusSign -> isUnreserved char || isReserved char
  _ -> isUnreserved char


-- | Percent encodes a character by UTF-8 encoding it and then percent encoding
-- the resulting octets.
percentEncodeChar :: Char -> String
percentEncodeChar = concatMap percentEncodeWord8 . encodeUtf8


-- | Percent encodes an octet by converting it into uppercase hexadecimal
-- digits and prepending a percent sign. For example @12@ becomes @"%0C"@.
percentEncodeWord8 :: Word.Word8 -> String
percentEncodeWord8 = Printf.printf "%%%02X"


-- | Expands an expression for output according to section 3.2 of the RFC,
-- using the given function to resolve variable values.
expandExpression
  :: Applicative m => (Name.Name -> m (Maybe Value.Value)) -> Expression.Expression -> m String
expandExpression f expression =
  let
    operator = Expression.operator expression
    prefix = prefixFor operator
    separator = separatorFor operator
    finalize expansions =
      (if null expansions then "" else prefix)
        <> List.intercalate separator expansions
  in fmap finalize . expandVariables f operator $ Expression.variables
    expression


-- | Returns the prefix to use before an expression for the given operator.
prefixFor :: Operator.Operator -> String
prefixFor operator = case operator of
  Operator.Ampersand -> "&"
  Operator.FullStop -> "."
  Operator.None -> ""
  Operator.NumberSign -> "#"
  Operator.PlusSign -> ""
  Operator.QuestionMark -> "?"
  Operator.Semicolon -> ";"
  Operator.Solidus -> "/"


-- | Returns the separator to use between values for the given operator.
separatorFor :: Operator.Operator -> String
separatorFor operator = case operator of
  Operator.Ampersand -> "&"
  Operator.FullStop -> "."
  Operator.None -> ","
  Operator.NumberSign -> ","
  Operator.PlusSign -> ","
  Operator.QuestionMark -> "&"
  Operator.Semicolon -> ";"
  Operator.Solidus -> "/"


-- | Expands variables for output according to section 3.2 of the RFC, using
-- the given function to resolve variable values.
expandVariables
  :: Applicative m
  => (Name.Name -> m (Maybe Value.Value))
  -> Operator.Operator
  -> NonEmpty.NonEmpty Variable.Variable
  -> m [String]
expandVariables f operator =
  fmap Maybe.catMaybes . traverse (expandVariable f operator) . NonEmpty.toList


-- | Expands a variable for output according to section 3.2.1 of the RFC, using
-- the given function to resolve variable values.
expandVariable
  :: Applicative m
  => (Name.Name -> m (Maybe Value.Value))
  -> Operator.Operator
  -> Variable.Variable
  -> m (Maybe String)
expandVariable f operator variable =
  let
    name = Variable.name variable
    modifier = Variable.modifier variable
  in expandMaybeValue operator name modifier <$> f name


-- | If the given value is not nothing, expand it according to section 3.2.1 of
-- the RFC.
expandMaybeValue :: Operator.Operator -> Name.Name -> Modifier.Modifier -> Maybe Value.Value -> Maybe String
expandMaybeValue operator name modifier maybeValue = do
  value <- maybeValue
  expandValue operator name modifier value


-- | Expands a value for output according to section 3.2.1 of the RFC. If the
-- value is undefined according to section 2.3, this returns nothing.
expandValue :: Operator.Operator -> Name.Name -> Modifier.Modifier -> Value.Value -> Maybe String
expandValue operator name modifier value = case value of
  Value.Dictionary dictionary ->
    expandDictionary operator name modifier <$> NonEmpty.fromList dictionary
  Value.List list ->
    expandList operator name modifier <$> NonEmpty.fromList list
  Value.String string -> Just $ expandString operator name modifier string


-- | Expands a dictionary (associative array) value for output.
expandDictionary
  :: Operator.Operator
  -> Name.Name
  -> Modifier.Modifier
  -> NonEmpty.NonEmpty (String, String)
  -> String
expandDictionary = expandElements
  $ \operator _ modifier -> expandDictionaryElement operator modifier


-- | Expands one element of a dictionary value for output.
expandDictionaryElement :: Operator.Operator -> Modifier.Modifier -> (String, String) -> [String]
expandDictionaryElement operator modifier (name, value) =
  let escape = escapeString operator Modifier.None
  in
    case modifier of
      Modifier.Asterisk -> [escape name <> "=" <> escape value]
      _ -> [escape name, escape value]


-- | Expands a list value for output.
expandList
  :: Operator.Operator -> Name.Name -> Modifier.Modifier -> NonEmpty.NonEmpty String -> String
expandList = expandElements $ \operator name modifier ->
  pure . expandListElement operator name modifier


-- | Expands one element of a list value for output.
expandListElement :: Operator.Operator -> Name.Name -> Modifier.Modifier -> String -> String
expandListElement operator name modifier = case modifier of
  Modifier.Asterisk -> expandString operator name Modifier.None
  _ -> expandString Operator.None name Modifier.None


-- | Expands a collection of elements for output. This is used for both
-- dictionaries and lists.
expandElements
  :: (Operator.Operator -> Name.Name -> Modifier.Modifier -> a -> [String])
  -> Operator.Operator
  -> Name.Name
  -> Modifier.Modifier
  -> NonEmpty.NonEmpty a
  -> String
expandElements f operator name modifier =
  let
    showPrefix = case modifier of
      Modifier.Asterisk -> False
      _ -> case operator of
        Operator.Ampersand -> True
        Operator.QuestionMark -> True
        Operator.Semicolon -> True
        _ -> False
    prefix = if showPrefix then nameToString name <> "=" else ""
    separator = case modifier of
      Modifier.Asterisk -> separatorFor operator
      _ -> ","
  in mappend prefix . List.intercalate separator . concatMap
    (f operator name modifier) . NonEmpty.toList


-- | Expands a string value for output.
expandString :: Operator.Operator -> Name.Name -> Modifier.Modifier -> String -> String
expandString operator name modifier s =
  let
    prefix = case operator of
      Operator.Ampersand -> nameToString name <> "="
      Operator.QuestionMark -> nameToString name <> "="
      Operator.Semicolon -> nameToString name <> if null s then "" else "="
      _ -> ""
  in prefix <> escapeString operator modifier s


-- | Escapes a string value for output. This handles encoding characters as
-- necessary for the given oeprator, as well as taking the prefix as necessary
-- for the given modifier.
escapeString :: Operator.Operator -> Modifier.Modifier -> String -> String
escapeString operator modifier string =
  concatMap (escapeChar operator) $ case modifier of
    Modifier.Colon size -> take size string
    _ -> string


-- | Converts a name into a regular string.
nameToString :: Name.Name -> String
nameToString = NonEmpty.toList . Name.chars


-- | Encodes a character as a series of UTF-8 octets. The resulting list will
-- have between one and four elements.
encodeUtf8 :: Char -> [Word.Word8]
encodeUtf8 char =
  let
    oneByte x = [intToWord8 $ bitAnd 0x7f x]
    twoBytes x =
      [ bitOr 0xc0 . intToWord8 . bitAnd 0x3f $ bitShiftR 6 x
      , bitOr 0x80 . intToWord8 $ bitAnd 0x3f x
      ]
    threeBytes x =
      [ bitOr 0xe0 . intToWord8 . bitAnd 0x0f $ bitShiftR 12 x
      , bitOr 0x80 . intToWord8 . bitAnd 0x3f $ bitShiftR 6 x
      , bitOr 0x80 . intToWord8 $ bitAnd 0x3f x
      ]
    fourBytes x =
      [ bitOr 0xf0 . intToWord8 . bitAnd 0x07 $ bitShiftR 18 x
      , bitOr 0x80 . intToWord8 . bitAnd 0x3f $ bitShiftR 12 x
      , bitOr 0x80 . intToWord8 . bitAnd 0x3f $ bitShiftR 6 x
      , bitOr 0x80 . intToWord8 $ bitAnd 0x3f x
      ]
  in case Char.ord char of
    int
      | int <= 0x7f -> oneByte int
      | int <= 0x7ff -> twoBytes int
      | int <= 0xffff -> threeBytes int
      | otherwise -> fourBytes int


-- | Computes the bitwise AND of the two parameters.
bitAnd :: Bits.Bits a => a -> a -> a
bitAnd = (Bits..&.)


-- | Computes the bitwise OR of the two parameters.
bitOr :: Bits.Bits a => a -> a -> a
bitOr = (Bits..|.)


-- | Shifts the given value to the right by the specified number of bits. If
-- the shift amount is negative, an exception will be thrown.
bitShiftR :: Bits.Bits a => Int -> a -> a
bitShiftR = flip Bits.shiftR


-- | Converts a machine-sized signed integer into an eight-bit unsigned
-- integer. If the input is out of bounds, an exception will be thrown.
intToWord8 :: Int -> Word.Word8
intToWord8 x =
  let
    lo = word8ToInt (minBound :: Word.Word8)
    hi = word8ToInt (maxBound :: Word.Word8)
  in if x < lo
    then error $ "intToWord8: " <> show x <> " < " <> show lo
    else if x > hi
      then error $ "intToWord8: " <> show x <> " > " <> show hi
      else fromIntegral x


-- | Converts an eight-bit unsigned integer into a machine-sized signed
-- integer. This conversion cannot fail.
word8ToInt :: Word.Word8 -> Int
word8ToInt = fromIntegral


-- | Returns true if the given character is in the @ALPHA@ range defined by
-- section 1.5 of the RFC.
isAlpha :: Char -> Bool
isAlpha x = Char.isAsciiUpper x || Char.isAsciiLower x


-- | Returns true if the given character is in the @reserved@ range defined by
-- section 1.5 of the RFC.
isReserved :: Char -> Bool
isReserved x = isGenDelim x || isSubDelim x


-- | Returns true if the given character is in the @gen-delims@ range defined
-- by section 1.5 of the RFC.
isGenDelim :: Char -> Bool
isGenDelim x = case x of
  ':' -> True
  '/' -> True
  '?' -> True
  '#' -> True
  '[' -> True
  ']' -> True
  '@' -> True
  _ -> False


-- | Returns true if the given character is in the @sub-delims@ range defined
-- by section 1.5 of the RFC.
isSubDelim :: Char -> Bool
isSubDelim x = case x of
  '!' -> True
  '$' -> True
  '&' -> True
  '\'' -> True
  '(' -> True
  ')' -> True
  '*' -> True
  '+' -> True
  ',' -> True
  ';' -> True
  '=' -> True
  _ -> False


-- | Returns true if the given character is in the @unreserved@ range defined
-- by section 1.5 of the RFC.
isUnreserved :: Char -> Bool
isUnreserved x = case x of
  '-' -> True
  '.' -> True
  '_' -> True
  '~' -> True
  _ -> isAlpha x || Char.isDigit x
