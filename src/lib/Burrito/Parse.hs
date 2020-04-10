module Burrito.Parse
  ( parse
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
import qualified Burrito.Type.Variable as Variable
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word


-- | Attempts to parse a string as a URI template. If parsing fails, this will
-- return 'Nothing'. Otherwise it will return 'Just' the parsed template.
--
-- Parsing will usually succeed, but it can fail if the input string contains
-- characters that are not valid in IRIs (like @^@) or if the input string
-- contains an invalid template expression (like @{!}@). To include characters
-- that aren't valid in IRIs, percent encode them (like @%5E@).
parse :: String -> Maybe Template.Template
parse string = case runParser parseTemplate string of
  Just (template, "") -> Just template
  _ -> Nothing


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


-- | Parses the given character and throws it away. See 'parseChar'.
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
parseTemplate :: Parser Template.Template
parseTemplate = Template.Template <$> Applicative.many parseToken


-- | Parses a token, which we define as part of a URI template.
parseToken :: Parser Token.Token
parseToken = parseEither
  (Token.Literal <$> parseLiteral)
  (Token.Expression <$> parseExpression)


-- | Parses a @literals@ value as defined by section 2.1 of the RFC.
parseLiteral :: Parser Literal.Literal
parseLiteral = Literal.Literal <$> parseNonEmpty parseCharacter


-- | Parses a character in a literal.
parseCharacter :: Parser Character.Character
parseCharacter = parseEither parseCharacterUnencoded parseCharacterEncoded


-- | Parses an unencoded character in a literal.
parseCharacterUnencoded :: Parser Character.Character
parseCharacterUnencoded = Character.Unencoded <$> parseIf isLiteral


-- | Parses a percent-encoded character in a literal.
parseCharacterEncoded :: Parser Character.Character
parseCharacterEncoded = do
  (hi, lo) <- parsePercentEncoded
  pure . Character.Encoded $ intToWord8
    (Char.digitToInt hi * 16 + Char.digitToInt lo)


-- | Parses an @expression@ as defined by section 2.2 of the RFC.
parseExpression :: Parser Expression.Expression
parseExpression =
  parseBetween (parseChar_ '{') (parseChar_ '}')
    $ Expression.Expression
    <$> parseOperator
    <*> parseVariableList


-- | Parses a @variable-list@ as defined by sections 2.3 of the RFC.
parseVariableList :: Parser (NonEmpty.NonEmpty Variable.Variable)
parseVariableList = parseSepBy1 (parseChar_ ',') parseVarspec


-- | Parses a @varspec@ as defined by section 2.3 of the RFC.
parseVarspec :: Parser Variable.Variable
parseVarspec = do
  name <- parseVarname
  modifier <- parseModifier
  pure $ Variable.Variable { Variable.name = name, Variable.modifier = modifier }


-- | Parses a @varname@ as defined by section 2.3 of the RFC.
parseVarname :: Parser Name.Name
parseVarname = do
  first <- parseVarcharFirst
  rest <- Applicative.many parseVarcharRest
  pure . Name.Name $ combine first rest


-- | Parses the first character in a variable name, which excludes periods.
parseVarcharFirst :: Parser (NonEmpty.NonEmpty Char)
parseVarcharFirst = parseEither parseVarcharUnencoded parseVarcharEncoded


-- | Parses an unencoded character in a variable name.
parseVarcharUnencoded :: Parser (NonEmpty.NonEmpty Char)
parseVarcharUnencoded = NonEmpty.singleton <$> parseIf isVarchar


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
  nonEmpty (NonEmpty.first xs)
    . mappend (NonEmpty.rest xs)
    . concatMap NonEmpty.toList


-- | Constructs a non-empty list without using an operator.
nonEmpty :: a -> [a] -> NonEmpty.NonEmpty a
nonEmpty = NonEmpty.NonEmpty


-- | Parses a @pct-encoded@ as defined by section 1.5 of the RFC. Returns both
-- hexadecimal digits as they appeared in the input without doing any case
-- normalization.
parsePercentEncoded :: Parser (Char, Char)
parsePercentEncoded = do
  parseChar_ '%'
  (,) <$> parseIf Char.isHexDigit <*> parseIf Char.isHexDigit


-- | Parses an @operator@ as defined by section 2.2 of the RFC.
parseOperator :: Parser Operator.Operator
parseOperator =
  Maybe.fromMaybe Operator.None <$> Applicative.optional parseRequiredOperator


-- | Parses a required, non-reserved operator as defined by section 2.2 of the
-- RFC. See 'parseOperator'.
parseRequiredOperator :: Parser Operator.Operator
parseRequiredOperator = do
  operator <- parseIf isOperator
  maybe Applicative.empty pure $ toOperator operator


-- | Converts an operator character into its respective operator type. Returns
-- nothing for characters that are not valid operators.
toOperator :: Char -> Maybe Operator.Operator
toOperator x = case x of
  '+' -> Just Operator.PlusSign
  '#' -> Just Operator.NumberSign
  '.' -> Just Operator.FullStop
  '/' -> Just Operator.Solidus
  ';' -> Just Operator.Semicolon
  '?' -> Just Operator.QuestionMark
  '&' -> Just Operator.Ampersand
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
parseModifier :: Parser Modifier.Modifier
parseModifier =
  fmap (Maybe.fromMaybe Modifier.None) . Applicative.optional $ parseEither
    parsePrefixModifier
    parseExplodeModifier


-- | Parses a @prefix@ as defined by section 2.4.1 of the RFC.
parsePrefixModifier :: Parser Modifier.Modifier
parsePrefixModifier = do
  parseChar_ ':'
  Modifier.Colon <$> parseMaxLength


-- | Parses a @max-length@ as defined by section 2.4.1 of the RFC.
parseMaxLength :: Parser Int
parseMaxLength = do
  first <- parseNonZeroDigit
  rest <- parseUpTo 3 parseDigit
  pure . fromDigits $ nonEmpty first rest


-- | Converts a list of digits into the number that they represent. For example
-- @[1, 2]@ becomes @12@.
fromDigits :: NonEmpty.NonEmpty Int -> Int
fromDigits = foldr1 ((+) . (10 *)) . NonEmpty.toList


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
parseExplodeModifier :: Parser Modifier.Modifier
parseExplodeModifier = Modifier.Asterisk <$ parseChar_ '*'


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
