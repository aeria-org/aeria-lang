module Aeria.Codegen.Typescript.Pretty where

import Prelude

import Aeria.Codegen.Typescript.Tree (TsIdentifier(..), TsImportSpecifier(..), TsParameter(..), TsSpecifiers(..), TsStatement(..), TsStatementSyntax(..), TsStatements(..), TsType(..), TsTypeLiteral(..), TsTypeObjectProperty(..), TsTypeParameter(..))
import Data.Array (length)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String.Utils (concatWith)

ppTypescript :: TsStatements -> String
ppTypescript (TsStatements stmts) =
  L.foldr (\s r -> ppStatement s <> "\n" <> r) "" stmts

ppStatement :: TsStatement -> String
ppStatement =
  case _ of
    TSImportDeclaration specifiers ident ->
      "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppIdentifier ident <> "\""
    TSVariableDeclaration statementSyntax ident type_ ->
       (L.foldr (\s r -> ppStatementSyntax s <> " " <> r) "" statementSyntax) <> ppIdentifier ident <> ": " <> ppType type_
    TSTypeAliasDeclaration ident type_ -> "declare type " <> ppIdentifier ident <> " = " <> ppType type_
    TSExportNamedDeclaration statement -> "export " <> ppStatement statement
    TSEmptyStatement -> ""

ppStatementSyntax :: TsStatementSyntax -> String
ppStatementSyntax =
  case _ of
    TsDeclareKeyword -> "declare"
    TsConstKeyword -> "const"

ppType :: TsType -> String
ppType =
  case _ of
    TSTypeAny -> "any"
    TSTypeString -> "string"
    TSTypeLiteral literal  -> ppLiteral literal
    TSTypeQuery ident -> "typeof " <> ppIdentifier ident
    TSIntersectionType lft rtg -> ppType lft <> " & " <> ppType rtg
    TSCallExpression called argum -> ppIdentifier called <> "(" <> concatWith argum ppType <> ")"
    TSTypeReference paramTypes ident -> ppIdentifier ident <> ppTypeParameters paramTypes
    TSFunctionType paramTypes param returnType ->
      ppTypeParameters paramTypes <>"(" <>
        concatWith param
        (\(TsParameter param' typ) -> ppIdentifier param' <> ": " <> ppType typ)
       <>  ") => " <> ppType returnType
    TSTypeExtends type1 type2 -> ppType type1 <> " extends " <> ppType type2

ppTypeParameters :: Array TsTypeParameter -> String
ppTypeParameters list
  | length list == 0 = ""
  | otherwise = "<"<> concatWith list ppTypeParameter <> ">"

ppTypeParameter :: TsTypeParameter -> String
ppTypeParameter (TsTypeParameter ident) = ppType ident

ppLiteral :: TsTypeLiteral -> String
ppLiteral =
  case _ of
    TSTypeLitString value -> "\"" <> value <> "\""
    TSTypeLitBoolean value -> show value
    TSTypeLitUndefined -> "undefined"
    TSTypeLitNull -> "null"
    TSTypeLitNumber value -> show value
    TSTypeLitArray value -> "[" <> concatWith value ppType <> "]"
    TSTypeLitObject value -> "{" <>
      concatWith value
        (\(TsTypeObjectProperty k v) -> ppIdentifier k <> ": " <> ppType v  ) <> "}"

ppSpecifiers :: TsSpecifiers -> String
ppSpecifiers (TsSpecifiers specifiers) = concatWith specifiers ppImportSpecifier

ppImportSpecifier :: TsImportSpecifier -> String
ppImportSpecifier (TsImportSpecifier importSpecifier Nothing) = ppIdentifier importSpecifier
ppImportSpecifier (TsImportSpecifier importSpecifier (Just alias)) = ppIdentifier importSpecifier <> " as " <> ppIdentifier alias

ppIdentifier :: TsIdentifier -> String
ppIdentifier (TsIdentifier ident) = ident
