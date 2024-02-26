module Aeria.Codegen.Javascript.Tree where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

data Output
  = CommonJs
  | EsNext

data JsTree
  = JSIdentifier String
  | JSNumberLiteral Number
  | JSStringLiteral String
  | JSBooleanLiteral Boolean
  | JSImport JsTree String
  | JSArrow (L.List String) JsTree
  | JSReturn JsTree
  | JSCall String (L.List JsTree)
  | JSDestructuringObject (L.List JsTree)
  | JSObjectLiteral (L.List (Tuple String JsTree))
  | JSArrayLiteral (L.List JsTree)
  | JSExport String JsTree
  | JSStatments (L.List JsTree)
  | JSCode String

derive instance genericJsTree :: Generic JsTree _

instance eqJsTree :: Eq JsTree where
  eq x = genericEq x

ppJsTree :: Output -> JsTree -> String
ppJsTree _ (JSCode code) = code
ppJsTree _ (JSIdentifier ident) = ident
ppJsTree _ (JSNumberLiteral number) = show number
ppJsTree _ (JSBooleanLiteral boolean) = show boolean
ppJsTree _ (JSStringLiteral string) = "\"" <> string <> "\""
ppJsTree out (JSObjectLiteral object) =
  let props = L.foldr (\(name /\ expr) rest -> name <> ":" <> ppJsTree out expr <> "," <> rest) "" object
  in "{" <> props <> "}"
ppJsTree out (JSArrayLiteral arr) =
  let values = L.foldr (\value rest -> ppJsTree out value <> "," <> rest) "" arr
  in "[" <> values <> "]"
ppJsTree out (JSExport name expr) =
  case out of
    CommonJs -> "exports." <> name <> " = " <> ppJsTree out expr
    EsNext -> "export const " <> name <> " = " <> ppJsTree out expr
ppJsTree out (JSStatments statments) = L.foldr (\statment rest -> ppJsTree out statment <> "\n" <> rest) "" statments
ppJsTree out (JSCall funct argum) =
  let argum' = L.foldr (\value rest -> ppJsTree out value <> "," <> rest) "" argum
  in funct <> "(" <> argum' <> ")"
ppJsTree out (JSArrow params body) =
  let params' = L.foldr (\value rest -> value <> "," <> rest) "" params
  in "(" <> params' <> ")" <> " => " <> ppJsTree out body
ppJsTree out (JSReturn return_) = "return" <> ppJsTree out return_
ppJsTree out (JSImport functions module_) =
  case out of
    CommonJs -> "const " <> ppJsTree out functions <> " = require(" <> show module_ <> ")"
    EsNext -> "import " <> ppJsTree out functions <> " from " <> show module_
ppJsTree out (JSDestructuringObject object) =
  let params' = L.foldr (\value rest -> ppJsTree out value <> "," <> rest) "" object
    in "{" <> params' <> "}"
