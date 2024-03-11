module Aeria.Codegen.Typescript.Pretty where

import Prelude

import Aeria.Codegen.Typescript.Tree (TsIdentifier(..), TsImportSpecifier(..), TsParameter(..), TsSpecifiers(..), TsStatement(..), TsStatementSyntax(..), TsStatements(..), TsType(..), TsTypeLiteral(..), TsTypeObjectProperty(..), TsTypeParameter(..))
import Data.List as L

ppTypescript :: TsStatements -> String
ppTypescript (TsStatements stmts) =
    L.foldr (\s r -> ppStatement s <> "\n" <> r) "" stmts

ppStatement :: TsStatement -> String
ppStatement =
  case _ of
    TSImportDeclaration specifiers ident ->
      "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppIdentifier ident <> "\""
    TSVariableStatement statementSyntax ident type_ ->
       (L.foldr (\s r -> ppStatementSyntax s <> " " <> r) "" statementSyntax) <> ppIdentifier ident <> ": " <> ppType type_
    TSTypeAliasDeclaration ident type_ -> "type " <> ppIdentifier ident <> " = " <> ppType type_
    TSExportNamedDeclaration statement -> "export " <> ppStatement statement

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
    TSCallExpression called argum -> ppIdentifier called <> "(" <> ppList argum ppType <> ")"
    TSTypeReference paramTypes ident -> ppIdentifier ident <> ppTypeParameters paramTypes
    TSFunctionType paramTypes param returnType ->
      ppTypeParameters paramTypes <>"(" <>
        ppList param
        (\(TsParameter param typ) -> ppIdentifier param <> ": " <> ppType typ)
       <>  ") => " <> ppType returnType
    TSTypeExtends type1 type2 -> ppType type1 <> " extends " <> ppType type2

ppTypeParameters :: L.List TsTypeParameter -> String
ppTypeParameters list
  | L.length list == 0 = ""
  | otherwise = "<"<> ppList list ppTypeParameter <> ">"

ppTypeParameter :: TsTypeParameter -> String
ppTypeParameter (TsTypeParameter ident) = ppType ident

ppLiteral :: TsTypeLiteral -> String
ppLiteral =
  case _ of
    TSTypeLitString value -> "\"" <> value <> "\""
    TSTypeLitBoolean value -> show value
    TSTypeLitNumber value -> show value
    TSTypeLitArray value -> "[" <> ppList value ppType <> "]"
    TSTypeLitObject value -> "{" <>
      ppList value
        (\(TsTypeObjectProperty k v) -> ppIdentifier k <> ": " <> ppType v  ) <> "}"

ppSpecifiers :: TsSpecifiers -> String
ppSpecifiers (TsSpecifiers specifiers) = ppList specifiers ppImportSpecifier

ppImportSpecifier :: TsImportSpecifier -> String
ppImportSpecifier (TsImportSpecifier importSpecifier) =
  ppIdentifier importSpecifier

ppIdentifier :: TsIdentifier -> String
ppIdentifier (TsIdentifier ident) = ident

-- ppList :: forall a. L.List a -> (a -> String) -> String
-- ppList list f =
--   L.foldr (\value rest -> f value <> "," <> rest) "" list

ppList :: forall a. L.List a  ->  (a -> String)-> String
ppList xs f  =
  xs
    # map f
    # L.intercalate ","
