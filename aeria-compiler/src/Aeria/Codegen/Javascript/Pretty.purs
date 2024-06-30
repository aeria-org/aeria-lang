module Aeria.Codegen.Javascript.Pretty where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsImportSpecifier(..), JsLiteral(..), JsObjectProperty(..), JsSpecifiers(..), JsStatement(..), JsStatements(..), JsTree(..), Module(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String.Utils (concatWith)

ppJavascript :: Module -> JsStatements -> String
ppJavascript module_ (JsStatements stmts) =
  L.foldr (\s r -> ppStatement module_ s <> "\n" <> r) "" stmts

ppStatement :: Module -> JsStatement -> String
ppStatement module_ =
  case _ of
    JSImportDeclaration specifiers ident ->
      case module_ of
        EsNext -> "import { " <> ppSpecifiers module_ specifiers <> " } from \"" <> ppTree ident <> "\""
        CommonJs -> "const {" <> ppSpecifiers module_ specifiers <> " } = require(\"" <> ppTree ident <> "\")"
    JSVariableDeclaration ident body ->
      case module_ of
        EsNext -> "const " <> ppTree ident <> " = " <> ppTree body
        CommonJs -> ppTree ident <> " = " <> ppTree body
    JSExportNamedDeclaration statement ->
      case module_ of
        EsNext -> "export " <> ppStatement module_ statement
        CommonJs -> "exports." <> ppStatement module_ statement

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
    JSUndefined -> "undefined"
    JSNull -> "null"
    JSNumber value -> show value
    JSBoolean value -> show value
    JSArray value -> "[" <> concatWith value ppTree <> "]"
    JSObject value -> "{" <> concatWith value
      (\property ->
        case property of
          (JsObjectProperty2 k v) -> ppTree k <> ": " <> ppTree v
          (JsObjectProperty1 k) -> ppTree k) <> "}"

ppSpecifiers :: Module -> JsSpecifiers -> String
ppSpecifiers module_ (JsSpecifiers specifiers) = concatWith specifiers (ppImportSpecifier module_)

ppImportSpecifier :: Module -> JsImportSpecifier -> String
ppImportSpecifier module_ (JsImportSpecifier importSpecifier alias) =
  case alias of
    Just alias' -> ppTree importSpecifier <>
      (case module_ of
        CommonJs -> " : "
        EsNext -> " as ")
      <> ppTree alias'
    Nothing -> ppTree importSpecifier
