module Aeria.Codegen.Javascript.Pretty where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsImportSpecifier(..), JsLiteral(..), JsObjectProperty(..), JsSpecifiers(..), JsStatement(..), JsStatements(..), JsTree(..), TargetModule(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String.Utils (concatWith)

ppJavascript :: TargetModule -> JsStatements -> String
ppJavascript targetModule (JsStatements stmts) =
  L.foldr (\s r -> ppStatement targetModule s <> "\n" <> r) "" stmts

ppStatement :: TargetModule -> JsStatement -> String
ppStatement targetModule =
  case _ of
    JSImportDeclaration specifiers ident ->
      case targetModule of
        EsNext -> "import { " <> ppSpecifiers targetModule specifiers <> " } from \"" <> ppTree ident <> "\""
        CommonJs -> "const {" <> ppSpecifiers targetModule specifiers <> " } = require(\"" <> ppTree ident <> "\")"
    JSVariableDeclaration ident body ->
      case targetModule of
        EsNext -> "const " <> ppTree ident <> " = " <> ppTree body
        CommonJs -> ppTree ident <> " = " <> ppTree body
    JSExportNamedDeclaration statement ->
      case targetModule of
        EsNext -> "export " <> ppStatement targetModule statement
        CommonJs -> "exports." <> ppStatement targetModule statement

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

ppSpecifiers :: TargetModule -> JsSpecifiers -> String
ppSpecifiers targetModule (JsSpecifiers specifiers) = concatWith specifiers (ppImportSpecifier targetModule)

ppImportSpecifier :: TargetModule -> JsImportSpecifier -> String
ppImportSpecifier targetModule (JsImportSpecifier importSpecifier alias) =
  case alias of
    Just alias' -> ppTree importSpecifier <>
      (case targetModule of
        CommonJs -> " : "
        EsNext -> " as ")
      <> ppTree alias'
    Nothing -> ppTree importSpecifier
