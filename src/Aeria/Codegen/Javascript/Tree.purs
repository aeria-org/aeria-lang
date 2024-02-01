module Aeria.Codegen.Javascript.Tree where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

data JSExpression
  = JSIdentifier String
  | JSNumberLiteral Number
  | JSStringLiteral String
  | JSBooleanLiteral Boolean
  | JSObjectLiteral (L.List (Tuple String JSExpression))
  | JSArrayLiteral (L.List JSExpression)
  | JSExports String JSExpression

derive instance genericJSExpression :: Generic JSExpression _

instance eqJSExpression :: Eq JSExpression where
  eq x = genericEq x

instance showJSExpression :: Show JSExpression where
  show (JSIdentifier ident) = ident
  show (JSNumberLiteral n) = show n
  show (JSBooleanLiteral b) = show b
  show (JSStringLiteral s) = "\"" <> s <> "\""
  show (JSObjectLiteral props) = "{" <> (L.foldr (\(name /\ expr) s -> name <> ":" <> show expr <> "," <> s) "" props) <> "}"
  show (JSArrayLiteral arr) = "[" <> L.foldr (\e s -> show e <> "," <> s) "" arr <> "]"
  show (JSExports name expr) = "exports." <> name <> " = " <> show expr
