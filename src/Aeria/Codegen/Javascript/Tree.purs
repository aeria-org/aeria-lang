module Aeria.Codegen.Javascript.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List as L

data Output
  = CommonJs
  | EsNext

data JsIdentifier
  = JsIdentifier String

derive instance genericJsIdentifier :: Generic JsIdentifier _

instance eqJsIdentifier :: Eq JsIdentifier where
  eq = genericEq

data JsLiteral
  = JSString String
  | JSNumber Number
  | JSBoolean Boolean
  | JSArray (L.List JsTree)
  | JSObject (L.List JsObjectProperty)

derive instance genericJsLiteral :: Generic JsLiteral _

instance eqJsLiteral :: Eq JsLiteral where
  eq = genericEq

data JsObjectProperty = JsObjectProperty JsIdentifier JsTree

derive instance genericJSObjectProperty :: Generic JsObjectProperty _

instance eqJsObjectProperty :: Eq JsObjectProperty where
  eq = genericEq

data JsImportSpecifier = JsImportSpecifier JsIdentifier

derive instance genericJsImportSpecifier :: Generic JsImportSpecifier _

instance eqJsImportSpecifier :: Eq JsImportSpecifier where
  eq = genericEq

data JsSpecifiers = JsSpecifiers (L.List JsImportSpecifier)

derive instance genericJsSpecifiers :: Generic JsSpecifiers _

instance eqJsSpecifiers :: Eq JsSpecifiers where
  eq = genericEq

data JsStatement
  = JSImportDeclaration JsSpecifiers JsIdentifier
  | JSVariableDeclaration JsIdentifier JsTree
  | JSExportNamedDeclaration JsStatement

derive instance genericJsStatement :: Generic JsStatement _

instance eqJsStatement :: Eq JsStatement where
  eq x = genericEq x

data JsStatements = JsStatements (L.List JsStatement)

derive instance genericJsStatements :: Generic JsStatements _

instance eqJsStatements :: Eq JsStatements where
  eq = genericEq

data JsTree
  = JSLiteral JsLiteral
  | JSIdentifier JsIdentifier
  | JSCallExpression JsIdentifier (L.List JsTree)
  | JSArrowFunctionExpression (L.List JsIdentifier) JsTree
  | JSCode String

derive instance genericJsTree :: Generic JsTree _

instance eqJsTree :: Eq JsTree where
  eq x = genericEq x
