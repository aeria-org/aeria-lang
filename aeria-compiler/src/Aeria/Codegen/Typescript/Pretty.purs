module Aeria.Codegen.Typescript.Pretty where

import Aeria.Codegen.Typescript.Tree
import Prelude (show, (<>))

import Data.List as L
import Data.String.Utils (concatWith)
import Data.Tuple.Nested ((/\))

ppTypescript :: TsStatements -> String
ppTypescript (TsStatements stmts) =
  L.foldr (\s r -> ppStatement s <> "\n" <> r) "" stmts

ppStatement :: TsStatement -> String
ppStatement =
  case _ of
    ImportDeclaration specifiers ident ->
      "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppIdentifier ident <> "\""
    VariableDeclaration ident type_ -> "const " <> ppIdentifier ident <> ": " <> ppTree type_
    TypeAliasDeclaration ident type_ -> "type " <> ppIdentifier ident <> " = " <> ppTree type_
    ExportDeclaration statement -> "export " <> ppStatement statement
    DeclareDeclaration statement -> "declare " <> ppStatement statement
    TSEmptyStatement -> ""

ppTree :: Tree -> String
ppTree =
  case _ of
    Type tsType -> ppType tsType
    Call ident args -> ppIdentifier ident <> "(" <> concatWith args ppTree <> ")"
    Variable ident -> ppIdentifier ident
    TypeQuery tree -> "typeof " <> ppTree tree
    Extends tree1 tree2 -> ppTree tree1 <> " extends " <> ppTree tree2
    Intersection tree1 tree2 -> ppTree tree1 <> " & " <> ppTree tree2

ppType :: TsType -> String
ppType TypeAny = "any"
ppType TypeString = "string"
ppType TypeNumber = "number"
ppType TypeBool = "boolean"
ppType (TypeRaw raw) = raw
ppType (TypeArray tsType) = ppType tsType <> "[]"
ppType (TypeLiteralNull) = "null"
ppType (TypeLiteralUndefined) = "undefined"
ppType (TypeLiteralString value) = "\"" <> value <> "\""
ppType (TypeLiteralBool value) = show value
ppType (TypeLiteralNumber value) = show value
ppType (TypeLiteralArray values) = "[" <> concatWith values ppTree <> "]"
ppType (TypeVariable ident) = ppIdentifier ident
ppType (TypeGeneric params tree) =
  case tree of
    Type (TypeFunction _ _) -> "<" <> concatWith params ppTree <> ">" <> ppTree tree
    _ -> ppTree tree <> "<" <> concatWith params ppTree <> ">"
ppType (TypeFunction params returnType) =
  "(" <> concatWith params ((\(ident /\ tree) -> ppIdentifier ident <> ": " <> ppTree tree)) <> ") => " <> ppTree returnType
ppType (TypeObject props) =
  "{" <> concatWith props (\(TypeObjectProperty ident tree) -> ppIdentifier ident <> ": " <> ppTree tree) <> "}"

ppSpecifiers :: Specifiers -> String
ppSpecifiers (Specifiers specifiers) = concatWith specifiers ppImportSpecifier

ppImportSpecifier :: ImportSpecifier -> String
ppImportSpecifier (ImportSpecifier ident) = ppIdentifier ident
ppImportSpecifier (ImportAliasSpecifier ident alias) = ppIdentifier ident <> " as " <> ppIdentifier alias

ppIdentifier :: Identifier -> String
ppIdentifier (Identifier ident) = ident
