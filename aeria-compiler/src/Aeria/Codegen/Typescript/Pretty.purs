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
ppStatement (ImportDeclaration specifiers ident) =
  "import { " <> ppSpecifiers specifiers <> " } from \"" <> ppIdentifier ident <> "\""
ppStatement (VariableDeclaration ident type_) =
  "const " <> ppIdentifier ident <> ": " <> ppTree type_
ppStatement (TypeAliasDeclaration ident type_) =
  "type " <> ppIdentifier ident <> " = " <> ppTree type_
ppStatement (ExportDeclaration statement) =
  "export " <> ppStatement statement
ppStatement (DeclareDeclaration statement) =
  "declare " <> ppStatement statement
ppStatement TSEmptyStatement = ""

ppTree :: Tree -> String
ppTree (Type tsType) = ppType tsType
ppTree (Call ident args) = ppIdentifier ident <> "(" <> concatWith args ppTree <> ")"
ppTree (Variable ident) = ppIdentifier ident
ppTree (TypeQuery tree) = "typeof " <> ppTree tree
ppTree (Extends tree1 tree2) = ppTree tree1 <> " extends " <> ppTree tree2
ppTree (Intersection tree1 tree2) = ppTree tree1 <> " & " <> ppTree tree2

ppType :: TsType -> String
ppType TypeAny = "any"
ppType TypeBool = "boolean"
ppType TypeString = "string"
ppType TypeNumber = "number"
ppType TypeLiteralNull = "null"
ppType TypeLiteralUndefined = "undefined"
ppType (TypeRaw raw) = raw
ppType (TypeArray tsType) = ppType tsType <> "[]"
ppType (TypeLiteralString value) = "\"" <> value <> "\""
ppType (TypeLiteralBool value) = show value
ppType (TypeLiteralNumber value) = show value
ppType (TypeLiteralArray values) = "[" <> concatWith values ppTree <> "]"
ppType (TypeVariable ident) = ppIdentifier ident
ppType (TypeGeneric params tree) =
  let paramsStr = "<" <> concatWith params ppTree <> ">"
  in case tree of
       Type (TypeFunction _ _) -> paramsStr <> ppTree tree
       _ -> ppTree tree <> paramsStr
ppType (TypeFunction params returnType) =
  "(" <> concatWith params (\(ident /\ tree) -> ppIdentifier ident <> ": " <> ppTree tree) <> ") => " <> ppTree returnType
ppType (TypeObject props) =
  "{" <> concatWith props (\(TypeObjectProperty ident tree) -> ppIdentifier ident <> ": " <> ppTree tree) <> "}"

ppSpecifiers :: Specifiers -> String
ppSpecifiers (Specifiers specifiers) = concatWith specifiers ppImportSpecifier

ppImportSpecifier :: ImportSpecifier -> String
ppImportSpecifier (ImportSpecifier ident) = ppIdentifier ident
ppImportSpecifier (ImportAliasSpecifier ident alias) = ppIdentifier ident <> " as " <> ppIdentifier alias

ppIdentifier :: Identifier -> String
ppIdentifier (Identifier ident) = ident
