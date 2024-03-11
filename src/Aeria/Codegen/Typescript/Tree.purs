module Aeria.Codegen.Typescript.Tree where

-- import Prelude

import Data.List as L

data TsTypeObjectProperty = TsTypeObjectProperty TsIdentifier TsType

data TsTypeLiteral
  = TSTypeLitString String
  | TSTypeLitBoolean Boolean
  | TSTypeLitArray (L.List TsType)
  | TSTypeLitNumber Number
  | TSTypeLitObject (L.List TsTypeObjectProperty)

data TsIdentifier = TsIdentifier String

data TsStatementSyntax
  = TsDeclareKeyword
  | TsConstKeyword

data TsTypeParameter = TsTypeParameter TsType

data TsParameter = TsParameter TsIdentifier TsType

data TsType
  = TSTypeAny
  | TSTypeString
  | TSTypeLiteral TsTypeLiteral
  | TSTypeQuery TsIdentifier
  | TSTypeExtends TsType TsType
  | TSIntersectionType TsType TsType
  | TSCallExpression TsIdentifier (L.List TsType)
  | TSTypeReference (L.List TsTypeParameter) TsIdentifier
  | TSFunctionType (L.List TsTypeParameter) (L.List TsParameter) TsType

data TsImportSpecifier = TsImportSpecifier TsIdentifier

data TsSpecifiers
  = TsSpecifiers (L.List TsImportSpecifier)

data TsStatement
  = TSImportDeclaration TsSpecifiers TsIdentifier
  | TSVariableStatement (L.List TsStatementSyntax) TsIdentifier TsType
  | TSTypeAliasDeclaration TsIdentifier TsType
  | TSExportNamedDeclaration TsStatement

data TsStatements
  = TsStatements (L.List TsStatement)
