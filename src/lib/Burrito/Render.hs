module Burrito.Render
  ( render
  )
where

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
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Text.Printf as Printf


-- | Renders a template back into a string. This is essentially the opposite of
-- @parse@.
render :: Template.Template -> String
render = concatMap renderToken . Template.tokens


-- | Renders a token in a template.
renderToken :: Token.Token -> String
renderToken token = case token of
  Token.Expression expression -> renderExpression expression
  Token.Literal literal -> renderLiteral literal


-- | Renders an expression token.
renderExpression :: Expression.Expression -> String
renderExpression expression = mconcat
  [ "{"
  , renderOperator $ Expression.operator expression
  , renderVariables $ Expression.variables expression
  , "}"
  ]


-- | Renders an operator in an expression.
renderOperator :: Operator.Operator -> String
renderOperator operator = case operator of
  Operator.Ampersand -> "&"
  Operator.FullStop -> "."
  Operator.None -> ""
  Operator.NumberSign -> "#"
  Operator.PlusSign -> "+"
  Operator.QuestionMark -> "?"
  Operator.Semicolon -> ";"
  Operator.Solidus -> "/"


-- | Renders a bunch of variables in an expression.
renderVariables :: NonEmpty.NonEmpty Variable.Variable -> String
renderVariables = List.intercalate "," . fmap renderVariable . NonEmpty.toList


-- | Renders a variable in an expression.
renderVariable :: Variable.Variable -> String
renderVariable variable = mconcat
  [ renderName $ Variable.name variable
  , renderModifier $ Variable.modifier variable
  ]


-- | Renders a variable name.
renderName :: Name.Name -> String
renderName = NonEmpty.toList . Name.chars


-- | Renders a variable modifier.
renderModifier :: Modifier.Modifier -> String
renderModifier modifier = case modifier of
  Modifier.Asterisk -> "*"
  Modifier.Colon int -> Printf.printf ":%d" int
  Modifier.None -> ""


-- | Renders a literal token.
renderLiteral :: Literal.Literal -> String
renderLiteral = concatMap renderCharacter . NonEmpty.toList . Literal.characters


-- | Renders a character in a literal token.
renderCharacter :: Character.Character -> String
renderCharacter character = case character of
  Character.Encoded word8 -> renderEncodedCharacter word8
  Character.Unencoded char -> renderUnencodedCharacter char


-- | Renders an encoded character by percent encoding it with uppercase
-- hexadecimal digits.
renderEncodedCharacter :: Word.Word8 -> String
renderEncodedCharacter = Printf.printf "%%%02X"


-- | Renders an unencoded character by simply turning it into a string.
renderUnencodedCharacter :: Char -> String
renderUnencodedCharacter = pure
