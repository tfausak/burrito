-- | Burrito is a Haskell library for parsing and rendering URI templates.
--
-- According to [RFC 6570](https://tools.ietf.org/html/rfc6570): "A URI
-- Template is a compact sequence of characters for describing a range of
-- Uniform Resource Identifiers through variable expansion." Burrito implements
-- URI templates according to the specification in that RFC.
--
-- The term "uniform resource identifiers" (URI) is often used interchangeably
-- with other related terms like "internationalized resource identifier" (IRI),
-- "uniform resource locator" (URL), and "uniform resource name" (URN). Burrito
-- can be used for all of these. If you want to get technical, its input must
-- be a valid IRI and its output will be a valid URI or URN.
--
-- Although Burrito is primarily intended to be used with HTTP and HTTPS URIs,
-- it should work with other schemes as well.
--
-- If you're not already familiar with URI templates, I recommend reading the
-- overview of the RFC. It's short, to the point, and easy to understand.
--
-- Assuming you're familiar with URI templates, here's a simple example to show
-- you how Burrito works:
--
-- > import Burrito
-- > let Just template = parse "http://example.com/search{?query}"
-- > expand [ ( "query", stringValue "bikes" ) ] template
-- > "http://example.com/search?query=bikes"
--
-- In short, use 'parse' to parse templates and 'expand' to render them.
module Burrito
  ( parse
  , expand
  , Template
  , Value
  , stringValue
  , listValue
  , dictionaryValue
  )
where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Text.Printf as Printf

#ifdef TemplateHaskell
import qualified Language.Haskell.TH.Syntax as TH
#endif

-- | Attempts to parse a string as a URI template. If parsing fails, this will
-- return 'Nothing'. Otherwise it will return 'Just' the parsed template.
--
-- Parsing will usually succeed, but it can fail if the input string contains
-- characters that are not valid in IRIs (like @^@) or if the input string
-- contains an invalid template expression (like @{!}@). To include characters
-- that aren't valid in IRIs, percent encode them (like @%5E@).
parse :: String -> Maybe Template
parse string = case runParser parseTemplate string of
  Just (template, "") -> Just template
  _ -> Nothing


-- | Expands a template using the given values. Unlike parsing, expansion
-- always succeeds. If no value is given for a variable, it will simply not
-- appear in the output.
expand :: [(String, Value)] -> Template -> String
expand values = Identity.runIdentity
  . expandTemplate (pure . flip lookup values . nameToString)


-- | Represents a URI template. Use 'parse' to create a template and 'expand'
-- to render one.
newtype Template = Template
  { template_tokens :: [Token]
  } deriving (Eq, Show)


-- | Represents a token in a template.
data Token
  = Token_Expression Expression
  | Token_Literal Literal
  deriving (Eq, Show)


-- | Represents a literal in a token.
newtype Literal = Literal
  { literal_characters :: NonEmpty.NonEmpty Character
  } deriving (Eq, Show)


-- | Represents a character in a literal. Although encoded characters are
-- allowed to have any value, typically they will not include most ASCII
-- printable characters. In other words @A@ is more likely than @%41@.
data Character
  = Character_Encoded Word.Word8
  | Character_Unencoded Char
  deriving (Eq, Show)


-- | Represents an expression in a token.
data Expression = Expression
  { expression_operator :: Operator
  , expression_variables :: NonEmpty.NonEmpty Variable
  } deriving (Eq, Show)


-- | Represents an operator in an expression.
data Operator
  = Operator_Ampersand
  | Operator_FullStop
  | Operator_None
  | Operator_NumberSign
  | Operator_PlusSign
  | Operator_QuestionMark
  | Operator_Semicolon
  | Operator_Solidus
  deriving (Eq, Show)


-- | Represents a variable in an expression.
data Variable = Variable
  { variable_modifier :: Modifier
  , variable_name :: Name
  } deriving (Eq, Show)


-- | Represents a modifier on a variable. The number associated with a prefix
-- modifier will be between 1 and 9999 inclusive.
data Modifier
  = Modifier_Asterisk
  | Modifier_Colon Int
  | Modifier_None
  deriving (Eq, Show)


-- | Represents a variable name, which is required to be non-empty. Variable
-- names allow ASCII letters and numbers, underscores, percent encoded triples,
-- and periods. However the periods cannot appear at the beginning or end, and
-- there can't be more than one of them in a row.
newtype Name = Name
  { name_chars :: NonEmpty.NonEmpty Char
  } deriving (Eq, Show)


-- | Represents a value that can be substituted into a template. Can be a
-- string, a list, or dictionary (which is called an associative array in the
-- RFC). Use 'stringValue', 'listValue', and 'dictionaryValue' to construct
-- values.
data Value
  = Value_Dictionary [(String, String)]
  | Value_List [String]
  | Value_String String
  deriving (Eq, Show)


-- | Constructs a string 'Value'.
stringValue :: String -> Value
stringValue = Value_String


-- | Constructs a list 'Value'.
listValue :: [String] -> Value
listValue = Value_List


-- | Constructs a dictionary 'Value'.
dictionaryValue :: [(String, String)] -> Value
dictionaryValue = Value_Dictionary


-- | Expands a template for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandTemplate
  :: Applicative m => (Name -> m (Maybe Value)) -> Template -> m String
expandTemplate f = expandTokens f . template_tokens


-- | Expands tokens for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandTokens
  :: Applicative m => (Name -> m (Maybe Value)) -> [Token] -> m String
expandTokens f = fmap concat . traverse (expandToken f)


-- | Expands a token for output according to section 3 of the RFC, using the
-- given function to resolve variable values.
expandToken :: Applicative m => (Name -> m (Maybe Value)) -> Token -> m String
expandToken f token = case token of
  Token_Literal literal -> pure $ expandLiteral literal
  Token_Expression expression -> expandExpression f expression


-- | Expands a literal token for output according to section 3.1 of the RFC.
expandLiteral :: Literal -> String
expandLiteral = concatMap expandCharacter . literal_characters


-- | Expands a single literal character for output. This is necessary to
-- normalize percent encodings and to encode characters that aren't allowed to
-- appear in URIs.
expandCharacter :: Character -> String
expandCharacter character = case character of
  Character_Encoded word8 -> percentEncodeWord8 word8
  Character_Unencoded char -> escapeChar Operator_PlusSign char


-- | If necessary, escapes a character for output with the given operator.
-- Otherwise returns the character unchanged as a string.
escapeChar :: Operator -> Char -> String
escapeChar operator char =
  if isAllowed operator char then [char] else percentEncodeChar char


-- | Returns true if the given character is allowed unescaped in the output for
-- the given operator.
isAllowed :: Operator -> Char -> Bool
isAllowed operator char = case operator of
  Operator_NumberSign -> isUnreserved char || isReserved char
  Operator_PlusSign -> isUnreserved char || isReserved char
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
  :: Applicative m => (Name -> m (Maybe Value)) -> Expression -> m String
expandExpression f expression =
  let
    operator = expression_operator expression
    prefix = prefixFor operator
    separator = separatorFor operator
    finalize expansions =
      (if null expansions then "" else prefix)
        <> List.intercalate separator expansions
  in fmap finalize . expandVariables f operator $ expression_variables
    expression


-- | Returns the prefix to use before an expression for the given operator.
prefixFor :: Operator -> String
prefixFor operator = case operator of
  Operator_Ampersand -> "&"
  Operator_FullStop -> "."
  Operator_None -> ""
  Operator_NumberSign -> "#"
  Operator_PlusSign -> ""
  Operator_QuestionMark -> "?"
  Operator_Semicolon -> ";"
  Operator_Solidus -> "/"


-- | Returns the separator to use between values for the given operator.
separatorFor :: Operator -> String
separatorFor operator = case operator of
  Operator_Ampersand -> "&"
  Operator_FullStop -> "."
  Operator_None -> ","
  Operator_NumberSign -> ","
  Operator_PlusSign -> ","
  Operator_QuestionMark -> "&"
  Operator_Semicolon -> ";"
  Operator_Solidus -> "/"


-- | Expands variables for output according to section 3.2 of the RFC, using
-- the given function to resolve variable values.
expandVariables
  :: Applicative m
  => (Name -> m (Maybe Value))
  -> Operator
  -> NonEmpty.NonEmpty Variable
  -> m [String]
expandVariables f operator =
  fmap Maybe.catMaybes . traverse (expandVariable f operator) . NonEmpty.toList


-- | Expands a variable for output according to section 3.2.1 of the RFC, using
-- the given function to resolve variable values.
expandVariable
  :: Applicative m
  => (Name -> m (Maybe Value))
  -> Operator
  -> Variable
  -> m (Maybe String)
expandVariable f operator variable =
  let
    name = variable_name variable
    modifier = variable_modifier variable
  in expandMaybeValue operator name modifier <$> f name


-- | If the given value is not nothing, expand it according to section 3.2.1 of
-- the RFC.
expandMaybeValue :: Operator -> Name -> Modifier -> Maybe Value -> Maybe String
expandMaybeValue operator name modifier maybeValue = do
  value <- maybeValue
  expandValue operator name modifier value


-- | Expands a value for output according to section 3.2.1 of the RFC. If the
-- value is undefined according to section 2.3, this returns nothing.
expandValue :: Operator -> Name -> Modifier -> Value -> Maybe String
expandValue operator name modifier value = case value of
  Value_Dictionary dictionary ->
    expandDictionary operator name modifier <$> NonEmpty.nonEmpty dictionary
  Value_List list ->
    expandList operator name modifier <$> NonEmpty.nonEmpty list
  Value_String string -> Just $ expandString operator name modifier string


-- | Expands a dictionary (associative array) value for output.
expandDictionary
  :: Operator
  -> Name
  -> Modifier
  -> NonEmpty.NonEmpty (String, String)
  -> String
expandDictionary = expandElements
  $ \operator _ modifier -> expandDictionaryElement operator modifier


-- | Expands one element of a dictionary value for output.
expandDictionaryElement :: Operator -> Modifier -> (String, String) -> [String]
expandDictionaryElement operator modifier (name, value) =
  let escape = escapeString operator Modifier_None
  in
    case modifier of
      Modifier_Asterisk -> [escape name <> "=" <> escape value]
      _ -> [escape name, escape value]


-- | Expands a list value for output.
expandList
  :: Operator -> Name -> Modifier -> NonEmpty.NonEmpty String -> String
expandList = expandElements $ \operator name modifier ->
  pure . expandListElement operator name modifier


-- | Expands one element of a list value for output.
expandListElement :: Operator -> Name -> Modifier -> String -> String
expandListElement operator name modifier = case modifier of
  Modifier_Asterisk -> expandString operator name Modifier_None
  _ -> expandString Operator_None name Modifier_None


-- | Expands a collection of elements for output. This is used for both
-- dictionaries and lists.
expandElements
  :: (Operator -> Name -> Modifier -> a -> [String])
  -> Operator
  -> Name
  -> Modifier
  -> NonEmpty.NonEmpty a
  -> String
expandElements f operator name modifier =
  let
    showPrefix = case modifier of
      Modifier_Asterisk -> False
      _ -> case operator of
        Operator_Ampersand -> True
        Operator_QuestionMark -> True
        Operator_Semicolon -> True
        _ -> False
    prefix = if showPrefix then nameToString name <> "=" else ""
    separator = case modifier of
      Modifier_Asterisk -> separatorFor operator
      _ -> ","
  in mappend prefix . List.intercalate separator . concatMap
    (f operator name modifier)


-- | Expands a string value for output.
expandString :: Operator -> Name -> Modifier -> String -> String
expandString operator name modifier s =
  let
    prefix = case operator of
      Operator_Ampersand -> nameToString name <> "="
      Operator_QuestionMark -> nameToString name <> "="
      Operator_Semicolon -> nameToString name <> if null s then "" else "="
      _ -> ""
  in prefix <> escapeString operator modifier s


-- | Escapes a string value for output. This handles encoding characters as
-- necessary for the given oeprator, as well as taking the prefix as necessary
-- for the given modifier.
escapeString :: Operator -> Modifier -> String -> String
escapeString operator modifier string =
  concatMap (escapeChar operator) $ case modifier of
    Modifier_Colon size -> take size string
    _ -> string


-- | Converts a name into a regular string.
nameToString :: Name -> String
nameToString = NonEmpty.toList . name_chars


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


-- | A simple type to handle parsing.
newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }


instance Functor Parser where
  -- | Applies the given function to the result of a successful parse.
  fmap f p = Parser $ \s -> case runParser p s of
    Nothing -> Nothing
    Just (x, t) -> Just (f x, t)


instance Applicative Parser where
  -- | Produces a parser that always succeeds by returning the given value.
  pure x = Parser $ \s -> Just (x, s)

  -- | Uses the first parser to get a function, then uses the second parser to
  -- get a value, then calls the function with the value.
  p <*> q = Parser $ \s -> case runParser p s of
    Nothing -> Nothing
    Just (f, t) -> case runParser q t of
      Nothing -> Nothing
      Just (x, u) -> Just (f x, u)


instance Monad Parser where
  -- | Feeds the output of a successful parse into the given function. If
  -- parsing fails, doesn't call the function.
  p >>= f = Parser $ \s -> case runParser p s of
    Nothing -> Nothing
    Just (x, t) -> runParser (f x) t


instance Applicative.Alternative Parser where
  -- | Fails without consuming any input.
  empty = Parser $ const Nothing

  -- | Returns the first parser if it succeeds. Otherwise returns the second
  -- parser.
  p <|> q = Parser $ \s -> case runParser p s of
    Nothing -> runParser q s
    Just (x, t) -> Just (x, t)


-- | Parses any one character. This is used as the basis for all other parsers.
parseAny :: Parser Char
parseAny = Parser $ \string -> case string of
  "" -> Nothing
  first : rest -> Just (first, rest)


-- | Runs the given parser between the other parsers. Useful for wrapping a
-- parser in quotes or parentheses.
parseBetween :: Parser before -> Parser after -> Parser a -> Parser a
parseBetween before after parser = before *> parser <* after


-- | Parses the given character and returns it.
parseChar :: Char -> Parser Char
parseChar = parseIf . (==)


-- | Parses the given character and throws it away. See 'parseChar'
parseChar_ :: Char -> Parser ()
parseChar_ = Monad.void . parseChar


-- | Tries to parse the first thing. If that fails, tries to parse the second
-- thing.
parseEither :: Parser a -> Parser a -> Parser a
parseEither = (Applicative.<|>)


-- | Parses one character if it passes the given predicate function.
parseIf :: (Char -> Bool) -> Parser Char
parseIf predicate = do
  char <- parseAny
  if predicate char then pure char else Applicative.empty


-- | Runs the given parser at least once.
parseNonEmpty :: Parser a -> Parser (NonEmpty.NonEmpty a)
parseNonEmpty parser = nonEmpty <$> parser <*> Applicative.many parser


-- | Runs the given parser separated by the other parser. Requires at least one
-- occurrence of the non-separator parser.
parseSepBy1 :: Parser separator -> Parser a -> Parser (NonEmpty.NonEmpty a)
parseSepBy1 separator parser =
  nonEmpty <$> parser <*> Applicative.many (separator *> parser)


-- | Parses a @URI-Template@ as defined by section 2 of the RFC.
parseTemplate :: Parser Template
parseTemplate = Template <$> Applicative.many parseToken


-- | Parses a token, which we define as part of a URI template.
parseToken :: Parser Token
parseToken = parseEither
  (Token_Literal <$> parseLiteral)
  (Token_Expression <$> parseExpression)


-- | Parses a @literals@ value as defined by section 2.1 of the RFC.
parseLiteral :: Parser Literal
parseLiteral = Literal <$> parseNonEmpty parseCharacter


-- | Parses a character in a literal.
parseCharacter :: Parser Character
parseCharacter = parseEither parseCharacterUnencoded parseCharacterEncoded


-- | Parses an unencoded character in a literal.
parseCharacterUnencoded :: Parser Character
parseCharacterUnencoded = Character_Unencoded <$> parseIf isLiteral


-- | Parses a percent-encoded character in a literal.
parseCharacterEncoded :: Parser Character
parseCharacterEncoded = do
  (hi, lo) <- parsePercentEncoded
  pure . Character_Encoded $ intToWord8
    (Char.digitToInt hi * 16 + Char.digitToInt lo)


-- | Parses an @expression@ as defined by section 2.2 of the RFC.
parseExpression :: Parser Expression
parseExpression =
  parseBetween (parseChar_ '{') (parseChar_ '}')
    $ Expression
    <$> parseOperator
    <*> parseVariableList


-- | Parses a @variable-list@ as defined by sections 2.3 of the RFC.
parseVariableList :: Parser (NonEmpty.NonEmpty Variable)
parseVariableList = parseSepBy1 (parseChar_ ',') parseVarspec


-- | Parses a @varspec@ as defined by section 2.3 of the RFC.
parseVarspec :: Parser Variable
parseVarspec = do
  name <- parseVarname
  modifier <- parseModifier
  pure $ Variable { variable_name = name, variable_modifier = modifier }


-- | Parses a @varname@ as defined by section 2.3 of the RFC.
parseVarname :: Parser Name
parseVarname = do
  first <- parseVarcharFirst
  rest <- Applicative.many parseVarcharRest
  pure . Name $ combine first rest


-- | Parses the first character in a variable name, which excludes periods.
parseVarcharFirst :: Parser (NonEmpty.NonEmpty Char)
parseVarcharFirst = parseEither parseVarcharUnencoded parseVarcharEncoded


-- | Parses an unencoded character in a variable name.
parseVarcharUnencoded :: Parser (NonEmpty.NonEmpty Char)
parseVarcharUnencoded = pure <$> parseIf isVarchar


-- | Parses a percent-encoded character in a variable name.
parseVarcharEncoded :: Parser (NonEmpty.NonEmpty Char)
parseVarcharEncoded = do
  (hi, lo) <- parsePercentEncoded
  pure $ nonEmpty '%' [hi, lo]


-- | Parses a non-first character in a variable name. This is like
-- 'parseVarcharFirst' except it allows periods.
parseVarcharRest :: Parser (NonEmpty.NonEmpty Char)
parseVarcharRest = parseEither
  (nonEmpty <$> parseChar '.' <*> fmap NonEmpty.toList parseVarcharFirst)
  parseVarcharFirst


-- | Returns true if the given character is in the @varchar@ range defined by
-- section 2.3 of the RFC. Note that this does not include the @pct-encoded@
-- part of the grammar because that requires multiple characters to match.
isVarchar :: Char -> Bool
isVarchar x = case x of
  '_' -> True
  _ -> isAlpha x || Char.isDigit x


-- | Adds a bunch of non-empty lists to the end of one non-empty list, while
-- keeping the non-emptiness around.
combine :: NonEmpty.NonEmpty a -> [NonEmpty.NonEmpty a] -> NonEmpty.NonEmpty a
combine xs =
  nonEmpty (NonEmpty.head xs)
    . mappend (NonEmpty.tail xs)
    . concatMap NonEmpty.toList


-- | Constructs a non-empty list without using an operator.
nonEmpty :: a -> [a] -> NonEmpty.NonEmpty a
nonEmpty = (NonEmpty.:|)


-- | Parses a @pct-encoded@ as defined by section 1.5 of the RFC. Returns both
-- hexadecimal digits as they appeared in the input without doing any case
-- normalization.
parsePercentEncoded :: Parser (Char, Char)
parsePercentEncoded = do
  parseChar_ '%'
  (,) <$> parseIf Char.isHexDigit <*> parseIf Char.isHexDigit


-- | Parses an @operator@ as defined by section 2.2 of the RFC.
parseOperator :: Parser Operator
parseOperator =
  Maybe.fromMaybe Operator_None <$> Applicative.optional parseRequiredOperator


-- | Parses a required, non-reserved operator as defined by section 2.2 of the
-- RFC. See 'parseOperator'.
parseRequiredOperator :: Parser Operator
parseRequiredOperator = do
  operator <- parseIf isOperator
  maybe Applicative.empty pure $ toOperator operator


-- | Converts an operator character into its respective 'Operator' type.
-- Returns nothing for characters that are not valid operators.
toOperator :: Char -> Maybe Operator
toOperator x = case x of
  '+' -> Just Operator_PlusSign
  '#' -> Just Operator_NumberSign
  '.' -> Just Operator_FullStop
  '/' -> Just Operator_Solidus
  ';' -> Just Operator_Semicolon
  '?' -> Just Operator_QuestionMark
  '&' -> Just Operator_Ampersand
  _ -> Nothing


-- | Returns true if the given character is in the @operator@ range defined by
-- section 2.2 of the RFC.
isOperator :: Char -> Bool
isOperator x = isOpLevel2 x || isOpLevel3 x || isOpReserve x


-- | Returns true if the given character is in the @op-level2@ range defined by
-- section 2.2 of the RFC.
isOpLevel2 :: Char -> Bool
isOpLevel2 x = case x of
  '+' -> True
  '#' -> True
  _ -> False


-- | Returns true if the given character is in the @op-level3@ range defined by
-- section 2.2 of the RFC.
isOpLevel3 :: Char -> Bool
isOpLevel3 x = case x of
  '.' -> True
  '/' -> True
  ';' -> True
  '?' -> True
  '&' -> True
  _ -> False


-- | Returns true if the given character is in the @op-reserve@ range defined
-- by section 2.2 of the RFC.
isOpReserve :: Char -> Bool
isOpReserve x = case x of
  '=' -> True
  ',' -> True
  '!' -> True
  '@' -> True
  '|' -> True
  _ -> False


-- | Parses a @modifier-level4@ as defined by section 2.4 of the RFC.
parseModifier :: Parser Modifier
parseModifier =
  fmap (Maybe.fromMaybe Modifier_None) . Applicative.optional $ parseEither
    parsePrefixModifier
    parseExplodeModifier


-- | Parses a @prefix@ as defined by section 2.4.1 of the RFC.
parsePrefixModifier :: Parser Modifier
parsePrefixModifier = do
  parseChar_ ':'
  Modifier_Colon <$> parseMaxLength


-- | Parses a @max-length@ as defined by section 2.4.1 of the RFC.
parseMaxLength :: Parser Int
parseMaxLength = do
  first <- parseNonZeroDigit
  rest <- parseUpTo 3 parseDigit
  pure . fromDigits $ nonEmpty first rest


-- | Converts a list of digits into the number that they represent. For example
-- @[1, 2]@ becomes @12@.
fromDigits :: NonEmpty.NonEmpty Int -> Int
fromDigits = foldr1 ((+) . (10 *))


-- | Parses up to the given number of occurrences of the given parser. If the
-- number is less than one, this will always succeed by returning the empty
-- list.
parseUpTo :: Int -> Parser a -> Parser [a]
parseUpTo = parseUpToWith []


-- | Like 'parseUpTo' but with an explicit accumulator.
parseUpToWith :: [a] -> Int -> Parser a -> Parser [a]
parseUpToWith accumulator remaining parser = if remaining < 1
  then pure accumulator
  else do
    result <- Applicative.optional parser
    case result of
      Nothing -> pure accumulator
      Just value -> parseUpToWith (value : accumulator) (remaining - 1) parser


-- | Parses a single non-zero decimal digit and returns that digit's value. See
-- 'isNonZeroDigit'.
parseNonZeroDigit :: Parser Int
parseNonZeroDigit = Char.digitToInt <$> parseIf isNonZeroDigit


-- | Returns true if the given character is a non-zero decimal digit. This
-- range isn't explicitly named by the RFC, but it's given in section 2.4.1.
isNonZeroDigit :: Char -> Bool
isNonZeroDigit x = case x of
  '0' -> False
  _ -> Char.isDigit x


-- | Parses a single decimal digit and returns that digit's value.
parseDigit :: Parser Int
parseDigit = Char.digitToInt <$> parseIf Char.isDigit


-- | Returns true if the given character is in the @ALPHA@ range defined by
-- section 1.5 of the RFC.
isAlpha :: Char -> Bool
isAlpha x = Char.isAsciiUpper x || Char.isAsciiLower x


-- | Parses an @explode@ as defined by section 2.4.2 of the RFC.
parseExplodeModifier :: Parser Modifier
parseExplodeModifier = Modifier_Asterisk <$ parseChar_ '*'


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


-- | Returns true if the given character is in the @literal@ range defined by
-- section 2.1 of the RFC.
isLiteral :: Char -> Bool
isLiteral x = case x of
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
  _ -> between '\x20' '\x7e' x || isUcschar x || isIprivate x


-- | Returns true if the given character is in the @ucschar@ range defined by
-- section 1.5 of the RFC.
isUcschar :: Char -> Bool
isUcschar x =
  between '\xa0' '\xd7ff' x
    || between '\xf900' '\xfdcf' x
    || between '\xfdf0' '\xffef' x
    || between '\x10000' '\x1fffd' x
    || between '\x20000' '\x2fffd' x
    || between '\x30000' '\x3fffd' x
    || between '\x40000' '\x4fffd' x
    || between '\x50000' '\x5fffd' x
    || between '\x60000' '\x6fffd' x
    || between '\x70000' '\x7fffd' x
    || between '\x80000' '\x8fffd' x
    || between '\x90000' '\x9fffd' x
    || between '\xa0000' '\xafffd' x
    || between '\xb0000' '\xbfffd' x
    || between '\xc0000' '\xcfffd' x
    || between '\xd0000' '\xdfffd' x
    || between '\xe1000' '\xefffd' x


-- | Returns true if the given character is in the @iprivate@ range defined by
-- section 1.5 of the RFC.
isIprivate :: Char -> Bool
isIprivate x =
  between '\xe000' '\xf8ff' x
    || between '\xf0000' '\xffffd' x
    || between '\x100000' '\x10fffd' x


-- | Returns true if the value is between the given inclusive bounds.
between
  :: Ord a
  => a -- ^ lower bound
  -> a -- ^ upper bound
  -> a
  -> Bool
between lo hi x = lo <= x && x <= hi

#ifdef TemplateHaskell
-- Derived Lift instance for Template Haskell
deriving instance TH.Lift Character
deriving instance TH.Lift Expression
deriving instance TH.Lift Literal
deriving instance TH.Lift Modifier
deriving instance TH.Lift Name
deriving instance TH.Lift Operator
deriving instance TH.Lift Template
deriving instance TH.Lift Token
deriving instance TH.Lift Variable
#endif
