module Aeria.Codegen.Javascript.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)

data Output
  = CommonJs
  | EsNext

data JsLiteral
  = JSString String
  | JSNumber Number
  | JSBoolean Boolean
  | JSArray (Array JsTree)
  | JSObject (Array JsObjectProperty)

derive instance genericJsLiteral :: Generic JsLiteral _

instance eqJsLiteral :: Eq JsLiteral where
  eq = genericEq

data JsObjectProperty
  = JsObjectProperty1 JsTree
  | JsObjectProperty2 JsTree JsTree

derive instance genericJSObjectProperty :: Generic JsObjectProperty _

instance eqJsObjectProperty :: Eq JsObjectProperty where
  eq = genericEq

data JsImportSpecifier = JsImportSpecifier JsTree

derive instance genericJsImportSpecifier :: Generic JsImportSpecifier _

instance eqJsImportSpecifier :: Eq JsImportSpecifier where
  eq = genericEq

data JsSpecifiers = JsSpecifiers (Array JsImportSpecifier)

derive instance genericJsSpecifiers :: Generic JsSpecifiers _

instance eqJsSpecifiers :: Eq JsSpecifiers where
  eq = genericEq

data JsStatement
  = JSImportDeclaration JsSpecifiers JsTree
  | JSVariableDeclaration JsTree JsTree
  | JSExportNamedDeclaration JsStatement

derive instance genericJsStatement :: Generic JsStatement _

instance eqJsStatement :: Eq JsStatement where
  eq x = genericEq x

data JsStatements = JsStatements (Array JsStatement)

derive instance genericJsStatements :: Generic JsStatements _

instance eqJsStatements :: Eq JsStatements where
  eq = genericEq

data JsTree
  = JSLiteral JsLiteral
  | JSIdentifier String
  | JSCallExpression JsTree (Array JsTree)
  | JSArrowFunctionExpression (Array JsTree) JsTree
  | JSCode String

derive instance genericJsTree :: Generic JsTree _

instance eqJsTree :: Eq JsTree where
  eq x = genericEq x

objectProperty2 :: String -> JsTree -> JsObjectProperty
objectProperty2 ident tree = JsObjectProperty2 (identifier ident) tree

objectProperty1 :: String -> JsObjectProperty
objectProperty1 ident = JsObjectProperty1 (identifier ident)

object :: Array JsObjectProperty -> JsTree
object o = JSLiteral (JSObject o)

array :: Array JsTree -> JsTree
array a = JSLiteral (JSArray a)

string :: String -> JsTree
string s = JSLiteral (JSString s)

boolean :: Boolean -> JsTree
boolean b = JSLiteral (JSBoolean b)

float :: Number -> JsTree
float f = JSLiteral (JSNumber f)

int :: Int -> JsTree
int i = JSLiteral (JSNumber (toNumber i))

identifier :: String -> JsTree
identifier s = JSIdentifier s

code :: String -> JsTree
code = JSCode

arrowFunction :: Array JsTree -> JsTree -> JsTree
arrowFunction = JSArrowFunctionExpression

call :: JsTree -> Array JsTree -> JsTree
call = JSCallExpression

import_ ∷ JsSpecifiers → JsTree → JsStatement
import_ = JSImportDeclaration

variable ∷ JsTree → JsTree → JsStatement
variable = JSVariableDeclaration

exportNamed ∷ JsStatement → JsStatement
exportNamed = JSExportNamedDeclaration

specifiers :: Array JsImportSpecifier -> JsSpecifiers
specifiers = JsSpecifiers

importSpecifier :: JsTree -> JsImportSpecifier
importSpecifier = JsImportSpecifier

statements ∷ Array JsStatement → JsStatements
statements = JsStatements
