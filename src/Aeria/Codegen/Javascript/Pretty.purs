module Aeria.Codegen.Javascript.Pretty where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsImportSpecifier(..), JsLiteral(..), JsObjectProperty(..), JsSpecifiers(..), JsStatement(..), JsStatements(..), JsTree(..), Output(..))
import Data.String.Utils (concatWith)
import Data.List as L

ppJavascript :: Output -> JsStatements -> String
ppJavascript output (JsStatements stmts) =
  L.foldr (\s r -> ppStatement output s <> "\n" <> r) "" stmts

ppStatement :: Output -> JsStatement -> String
ppStatement output =
  case _ of
    JSImportDeclaration specifiers ident ->
      case output of
        EsNext -> "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppTree ident <> "\""
        CommonJs -> "const {" <> ppSpecifiers specifiers <> " } = require(\"" <> ppTree ident <> "\")"
    JSVariableDeclaration ident body ->
      case output of
        EsNext -> "const " <> ppTree ident <> " = " <> ppTree body
        CommonJs -> ppTree ident <> " = " <> ppTree body
    JSExportNamedDeclaration statement ->
      case output of
        EsNext -> "export " <> ppStatement output statement
        CommonJs -> "exports." <> ppStatement output statement

ppTree :: JsTree -> String
ppTree =
  case _ of
    JSLiteral literal -> ppLiteral literal
    JSIdentifier ident -> ident
    JSCallExpression called params -> ppTree called <> "("<> concatWith params ppTree  <>")"
    JSArrowFunctionExpression argum body -> "("<> concatWith argum ppTree <>") => " <> ppTree body
    JSCode code -> code

ppLiteral :: JsLiteral -> String
ppLiteral =
  case _ of
    JSString value -> "\"" <> value <> "\""
    JSNumber value -> show value
    JSBoolean value -> show value
    JSArray value -> "[" <> concatWith value ppTree <> "]"
    JSObject value -> "{" <> concatWith value
      (\property ->
        case property of
          (JsObjectProperty2 k v) -> ppTree k <> ": " <> ppTree v
          (JsObjectProperty1 k) -> ppTree k) <> "}"

ppSpecifiers :: JsSpecifiers -> String
ppSpecifiers (JsSpecifiers specifiers) = concatWith specifiers ppImportSpecifier

ppImportSpecifier :: JsImportSpecifier -> String
ppImportSpecifier (JsImportSpecifier importSpecifier) =
  ppTree importSpecifier
