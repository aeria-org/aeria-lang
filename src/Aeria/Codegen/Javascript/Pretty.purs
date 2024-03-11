module Aeria.Codegen.Javascript.Pretty where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsIdentifier(..), JsImportSpecifier(..), JsLiteral(..), JsObjectProperty(..), JsSpecifiers(..), JsStatement(..), JsStatements(..), JsTree(..), Output(..))
import Data.List as L

ppJavascript :: Output -> JsStatements -> String
ppJavascript output (JsStatements stmts) =
  L.foldr (\s r -> ppStatement output s <> "\n" <> r) "" stmts

ppStatement :: Output -> JsStatement -> String
ppStatement output =
  case _ of
    JSImportDeclaration specifiers ident ->
      case output of
        EsNext -> "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppIdentifier ident <> "\""
        CommonJs -> "const {" <> ppSpecifiers specifiers <> " } = require(\"" <> ppIdentifier ident <> "\")"
    JSVariableDeclaration ident body ->
      case output of
        EsNext -> "const " <> ppIdentifier ident <> " = " <> ppTree body
        CommonJs -> ppIdentifier ident <> " = " <> ppTree body
    JSExportNamedDeclaration statement ->
      case output of
        EsNext -> "export " <> ppStatement output statement
        CommonJs -> "exports." <> ppStatement output statement

ppTree :: JsTree -> String
ppTree =
  case _ of
    JSLiteral literal -> ppLiteral literal
    JSIdentifier ident -> ppIdentifier ident
    JSCallExpression called params -> ppIdentifier called <> "("<> ppList params ppTree  <>")"
    JSArrowFunctionExpression argum body -> "("<> ppList argum ppIdentifier <>") => " <> ppTree body
    JSCode code -> code

ppLiteral :: JsLiteral -> String
ppLiteral =
  case _ of
    JSString value -> "\"" <> value <> "\""
    JSNumber value -> show value
    JSBoolean value -> show value
    JSArray value -> "[" <> ppList value ppTree <> "]"
    JSObject value -> "{" <> ppList value
      (\(JsObjectProperty k v) -> ppIdentifier k <> ": " <> ppTree v  ) <> "}"

ppSpecifiers :: JsSpecifiers -> String
ppSpecifiers (JsSpecifiers specifiers) = ppList specifiers ppImportSpecifier

ppImportSpecifier :: JsImportSpecifier -> String
ppImportSpecifier (JsImportSpecifier importSpecifier) =
  ppIdentifier importSpecifier

ppIdentifier :: JsIdentifier -> String
ppIdentifier (JsIdentifier ident) = ident

ppList :: forall a. L.List a  ->  (a -> String)-> String
ppList xs f  =
  xs
    # map f
    # L.intercalate ","
