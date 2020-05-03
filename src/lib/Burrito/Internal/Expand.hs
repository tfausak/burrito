module Burrito.Internal.Expand where

import qualified Burrito.Internal.Render as Render
import qualified Burrito.Internal.Type.Character as Character
import qualified Burrito.Internal.Type.Digit as Digit
import qualified Burrito.Internal.Type.Expression as Expression
import qualified Burrito.Internal.Type.Field as Field
import qualified Burrito.Internal.Type.Literal as Literal
import qualified Burrito.Internal.Type.MaxLength as MaxLength
import qualified Burrito.Internal.Type.Modifier as Modifier
import qualified Burrito.Internal.Type.Name as Name
import qualified Burrito.Internal.Type.NonEmpty as NonEmpty
import qualified Burrito.Internal.Type.Operator as Operator
import qualified Burrito.Internal.Type.Template as Template
import qualified Burrito.Internal.Type.Token as Token
import qualified Burrito.Internal.Type.Value as Value
import qualified Burrito.Internal.Type.Variable as Variable
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder

expand :: [(String, Value.Value)] -> Template.Template -> String
expand values =
  let m = Map.mapKeys Text.pack $ Map.fromList values
  in
    LazyText.unpack . Builder.toLazyText . Identity.runIdentity . expandWith
      (pure . flip Map.lookup m)

expandWith
  :: Monad m
  => (Text.Text -> m (Maybe Value.Value))
  -> Template.Template
  -> m Builder.Builder
expandWith f = flip State.evalStateT Map.empty . template (cached f)

type CacheT = State.StateT (Map.Map Text.Text (Maybe Value.Value))

cached
  :: Monad m
  => (Text.Text -> m (Maybe Value.Value))
  -> Name.Name
  -> CacheT m (Maybe Value.Value)
cached f x = do
  let key = LazyText.toStrict . Builder.toLazyText $ name x
  cache <- State.get
  case Map.lookup key cache of
    Just result -> pure result
    Nothing -> do
      result <- Trans.lift $ f key
      State.modify $ Map.insert key result
      pure result

template
  :: Monad m
  => (Name.Name -> CacheT m (Maybe Value.Value))
  -> Template.Template
  -> CacheT m Builder.Builder
template f = fmap mconcat . traverse (token f) . Template.tokens

token
  :: Monad m
  => (Name.Name -> CacheT m (Maybe Value.Value))
  -> Token.Token
  -> CacheT m Builder.Builder
token f x = case x of
  Token.Expression y -> expression f y
  Token.Literal y -> pure $ literal y

expression
  :: Monad m
  => (Name.Name -> CacheT m (Maybe Value.Value))
  -> Expression.Expression
  -> CacheT m Builder.Builder
expression f ex =
  let op = Expression.operator ex
  in
    fmap
      (mconcat
      . (\xs -> if null xs then xs else prefix op : xs)
      . List.intersperse (separator op)
      . Maybe.catMaybes
      )
    . traverse (variable f op)
    . NonEmpty.toList
    $ Expression.variables ex

separator :: Operator.Operator -> Builder.Builder
separator op = Builder.singleton $ case op of
  Operator.Ampersand -> '&'
  Operator.FullStop -> '.'
  Operator.None -> ','
  Operator.NumberSign -> ','
  Operator.PlusSign -> ','
  Operator.QuestionMark -> '&'
  Operator.Semicolon -> ';'
  Operator.Solidus -> '/'

prefix :: Operator.Operator -> Builder.Builder
prefix op = case op of
  Operator.Ampersand -> Builder.singleton '&'
  Operator.FullStop -> Builder.singleton '.'
  Operator.None -> mempty
  Operator.NumberSign -> Builder.singleton '#'
  Operator.PlusSign -> mempty
  Operator.QuestionMark -> Builder.singleton '?'
  Operator.Semicolon -> Builder.singleton ';'
  Operator.Solidus -> Builder.singleton '/'

variable
  :: Monad m
  => (Name.Name -> CacheT m (Maybe Value.Value))
  -> Operator.Operator
  -> Variable.Variable
  -> CacheT m (Maybe Builder.Builder)
variable f op var = do
  res <- f $ Variable.name var
  pure $ case res of
    Nothing -> Nothing
    Just val -> value op var val

value
  :: Operator.Operator
  -> Variable.Variable
  -> Value.Value
  -> Maybe Builder.Builder
value op var val = case val of
  Value.Dictionary xs -> dictionaryValue op var $ Map.toAscList xs
  Value.List xs -> listValue op var xs
  Value.String x -> Just $ stringValue op var x

dictionaryValue
  :: Operator.Operator
  -> Variable.Variable
  -> [(Text.Text, Text.Text)]
  -> Maybe Builder.Builder
dictionaryValue = items $ \op var (k, v) ->
  let f = string op Modifier.None
  in
    case Variable.modifier var of
      Modifier.Asterisk -> [f k <> Builder.singleton '=' <> f v]
      _ -> [f k, f v]

listValue
  :: Operator.Operator
  -> Variable.Variable
  -> [Text.Text]
  -> Maybe Builder.Builder
listValue = items $ \op var -> pure . stringValue
  (case Variable.modifier var of
    Modifier.Asterisk -> op
    _ -> Operator.None
  )
  var { Variable.modifier = Modifier.None }

items
  :: (Operator.Operator -> Variable.Variable -> a -> [Builder.Builder])
  -> Operator.Operator
  -> Variable.Variable
  -> [a]
  -> Maybe Builder.Builder
items f op var xs =
  let
    md = Variable.modifier var
    sep = case md of
      Modifier.Asterisk -> separator op
      _ -> Builder.singleton ','
    p = case md of
      Modifier.Asterisk -> False
      _ -> case op of
        Operator.Ampersand -> True
        Operator.QuestionMark -> True
        Operator.Semicolon -> True
        _ -> False
  in if null xs
    then Nothing
    else
      Just
      . mconcat
      . (if p then (label True var :) else id)
      . List.intersperse sep
      $ concatMap (f op var) xs

label :: Bool -> Variable.Variable -> Builder.Builder
label p v =
  name (Variable.name v) <> if p then Builder.singleton '=' else mempty

name :: Name.Name -> Builder.Builder
name =
  mconcat
    . List.intersperse (Builder.singleton '.')
    . fmap field
    . NonEmpty.toList
    . Name.fields

field :: Field.Field -> Builder.Builder
field =
  mconcat . fmap (character $ const True) . NonEmpty.toList . Field.characters

character :: (Char -> Bool) -> Character.Character tag -> Builder.Builder
character f x = case x of
  Character.Encoded y z -> Render.encodedCharacter y z
  Character.Unencoded y -> unencodedCharacter f y

stringValue
  :: Operator.Operator -> Variable.Variable -> Text.Text -> Builder.Builder
stringValue op var str =
  let
    pre = case op of
      Operator.Ampersand -> label True var
      Operator.QuestionMark -> label True var
      Operator.Semicolon -> label (not $ Text.null str) var
      _ -> mempty
  in pre <> string op (Variable.modifier var) str

string
  :: Operator.Operator -> Modifier.Modifier -> Text.Text -> Builder.Builder
string op md =
  let
    allowed x = case op of
      Operator.NumberSign -> isAllowed x
      Operator.PlusSign -> isAllowed x
      _ -> isUnreserved x
    trim = case md of
      Modifier.Colon ml -> Text.take $ MaxLength.count ml
      _ -> id
  in foldMap (unencodedCharacter allowed) . Text.unpack . trim

isAllowed :: Char -> Bool
isAllowed x = isUnreserved x || isReserved x

isUnreserved :: Char -> Bool
isUnreserved x = case x of
  '-' -> True
  '.' -> True
  '_' -> True
  '~' -> True
  _ -> Char.isAsciiUpper x || Char.isAsciiLower x || Char.isDigit x

isReserved :: Char -> Bool
isReserved x = case x of
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
  ':' -> True
  '/' -> True
  '?' -> True
  '#' -> True
  '[' -> True
  ']' -> True
  '@' -> True
  _ -> False

unencodedCharacter :: (Char -> Bool) -> Char -> Builder.Builder
unencodedCharacter f x = if f x
  then Builder.singleton x
  else foldMap (uncurry Render.encodedCharacter) $ encodeCharacter x

encodeCharacter :: Char -> [(Digit.Digit, Digit.Digit)]
encodeCharacter =
  fmap Digit.fromWord8 . ByteString.unpack . Text.encodeUtf8 . Text.singleton

literal :: Literal.Literal -> Builder.Builder
literal = foldMap (character isAllowed) . NonEmpty.toList . Literal.characters
