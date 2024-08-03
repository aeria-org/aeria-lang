module Aeria.Codegen.Javascript.Pretty where

import Prelude (show, (<>))

import Aeria.Codegen.Javascript.Tree
import Data.List as L
import Data.String.Utils (concatWith)

ppJavascript :: TargetModule -> Statements -> String
ppJavascript targetModule (Statements stmts) =
  L.foldr (\s r -> ppStatement targetModule s <> "\n" <> r) "" stmts

ppStatement :: TargetModule -> Statement -> String
ppStatement targetModule (ImportDeclaration specifiers ident) =
  case targetModule of
    EsNext -> "import { " <> ppSpecifiers targetModule specifiers <> " } from \"" <> ppIdentifier ident <> "\""
    CommonJs -> "const {" <> ppSpecifiers targetModule specifiers <> " } = require(\"" <> ppIdentifier ident <> "\")"
ppStatement targetModule (VariableDeclaration ident body) =
  case targetModule of
    EsNext -> "const " <> ppIdentifier ident <> " = " <> ppTree body
    CommonJs -> ppIdentifier ident <> " = " <> ppTree body
ppStatement targetModule (ExportDeclaration statement) =
  case targetModule of
    EsNext -> "export " <> ppStatement targetModule statement
    CommonJs -> "exports." <> ppStatement targetModule statement

ppTree :: Tree -> String
ppTree (Literal literal) = ppLiteral literal
ppTree (Variable ident) = ppIdentifier ident
ppTree (Call called params) = ppTree called <> "(" <> concatWith params "," ppTree <> ")"
ppTree (Function args body) = "(" <> concatWith args "," ppIdentifier <> ") => " <> ppTree body
ppTree (Raw code) = code
ppTree (Object props) = "{" <> concatWith props "," ppObjectProperty <> "}"

ppObjectProperty :: ObjectProperty -> String
ppObjectProperty (ObjectProperty2 key value) = ppIdentifier key <> ": " <> ppTree value
ppObjectProperty (ObjectProperty1 key) = ppIdentifier key

ppLiteral :: Literal -> String
ppLiteral Null = "null"
ppLiteral Undefined = "undefined"
ppLiteral (String value) = "\"" <> value <> "\""
ppLiteral (Number value) = show value
ppLiteral (Boolean value) = show value
ppLiteral (Array values) = "[" <> concatWith values "," ppTree <> "]"

ppSpecifiers :: TargetModule -> Specifiers -> String
ppSpecifiers targetModule (Specifiers specifiers) = concatWith specifiers "," (ppImportSpecifier targetModule)

ppImportSpecifier :: TargetModule -> ImportSpecifier -> String
ppImportSpecifier targetModule (ImportAliasSpecifier importSpecifier alias) =
  ppIdentifier importSpecifier <> ppAliasSeparator targetModule <> ppIdentifier alias
ppImportSpecifier _ (ImportSpecifier importSpecifier) = ppIdentifier importSpecifier

ppAliasSeparator :: TargetModule -> String
ppAliasSeparator CommonJs = " : "
ppAliasSeparator EsNext = " as "

ppIdentifier :: Identifier -> String
ppIdentifier (Identifier ident) = ident
