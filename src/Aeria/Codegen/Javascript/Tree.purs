module Aeria.Codegen.Javascript.Tree where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

data JsTree
  = JSIdentifier String
  | JSNumberLiteral Number
  | JSStringLiteral String
  | JSBooleanLiteral Boolean
  | JSObjectLiteral (L.List (Tuple String JsTree))
  | JSArrayLiteral (L.List JsTree)
  | JSExport String JsTree
  | JSExports (L.List JsTree)
  | JSCode String

derive instance genericJsTree :: Generic JsTree _

instance eqJsTree :: Eq JsTree where
  eq x = genericEq x

instance showJsTree :: Show JsTree where
  show (JSIdentifier ident) = ident
  show (JSCode code) = code
  show (JSNumberLiteral n) = show n
  show (JSBooleanLiteral b) = show b
  show (JSStringLiteral s) = "\"" <> s <> "\""
  show (JSObjectLiteral props) = "{" <> (L.foldr (\(name /\ expr) s -> name <> ":" <> show expr <> "," <> s) "" props) <> "}"
  show (JSArrayLiteral arr) = "[" <> L.foldr (\e s -> show e <> "," <> s) "" arr <> "]"
  show (JSExport name expr) = "exports." <> name <> " = " <> show expr
  show (JSExports exports) = L.foldr (\a b -> show a <> "\n" <> b) "" exports
