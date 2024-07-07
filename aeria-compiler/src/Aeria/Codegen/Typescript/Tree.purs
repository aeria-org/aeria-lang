module Aeria.Codegen.Typescript.Tree where

import Data.Maybe (Maybe(..))

data TsTypeObjectProperty = TsTypeObjectProperty TsIdentifier TsType

data TsTypeLiteral
  = TSTypeLitString String
  | TSTypeLitBoolean Boolean
  | TSTypeLitUndefined
  | TSTypeLitNull
  | TSTypeLitArray (Array TsType)
  | TSTypeLitNumber Number
  | TSTypeLitObject (Array TsTypeObjectProperty)

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
  | TSCallExpression TsIdentifier (Array TsType)
  | TSTypeReference (Array TsTypeParameter) TsIdentifier
  | TSFunctionType (Array TsTypeParameter) (Array TsParameter) TsType

data TsImportSpecifier = TsImportSpecifier TsIdentifier (Maybe TsIdentifier)

data TsSpecifiers
  = TsSpecifiers (Array TsImportSpecifier)

data TsStatement
  = TSImportDeclaration TsSpecifiers TsIdentifier
  | TSVariableDeclaration (Array TsStatementSyntax) TsIdentifier TsType
  | TSTypeAliasDeclaration TsIdentifier TsType
  | TSExportNamedDeclaration TsStatement
  | TSEmptyStatement

data TsStatements
  = TsStatements (Array TsStatement)

statements ∷ Array TsStatement -> TsStatements
statements = TsStatements

import_ ∷ TsSpecifiers -> TsIdentifier -> TsStatement
import_ = TSImportDeclaration

variable ∷ Array TsStatementSyntax -> TsIdentifier -> TsType -> TsStatement
variable = TSVariableDeclaration

typeAlias ∷ TsIdentifier -> TsType -> TsStatement
typeAlias = TSTypeAliasDeclaration

exportNamed ∷ TsStatement -> TsStatement
exportNamed = TSExportNamedDeclaration

specifiers ∷ Array TsImportSpecifier -> TsSpecifiers
specifiers = TsSpecifiers

importSpecifier ∷ TsIdentifier -> TsImportSpecifier
importSpecifier x = TsImportSpecifier x Nothing

importSpecifier2 ∷ TsIdentifier -> TsIdentifier -> TsImportSpecifier
importSpecifier2 x y = TsImportSpecifier x (Just y)

typeAny ∷ TsType
typeAny = TSTypeAny

typeString ∷ TsType
typeString = TSTypeString

typeLiteral ∷ TsTypeLiteral -> TsType
typeLiteral = TSTypeLiteral

typeQuery ∷ TsIdentifier -> TsType
typeQuery = TSTypeQuery

typeExtends ∷ TsType -> TsType -> TsType
typeExtends = TSTypeExtends

intersectionType ∷ TsType -> TsType -> TsType
intersectionType = TSIntersectionType

callExpression ∷ TsIdentifier -> Array TsType -> TsType
callExpression = TSCallExpression

typeReference ∷ Array TsTypeParameter -> TsIdentifier -> TsType
typeReference = TSTypeReference

functionType ∷ Array TsTypeParameter -> Array TsParameter -> TsType -> TsType
functionType = TSFunctionType

parameter ∷ TsIdentifier -> TsType -> TsParameter
parameter = TsParameter

typeParameter ∷ TsType -> TsTypeParameter
typeParameter = TsTypeParameter

declareKeyword ∷ TsStatementSyntax
declareKeyword = TsDeclareKeyword

constKeyword ∷ TsStatementSyntax
constKeyword = TsConstKeyword

identifier ∷ String -> TsIdentifier
identifier = TsIdentifier

emptyStatement ∷ TsStatement
emptyStatement = TSEmptyStatement

typeLitString ∷ String -> TsTypeLiteral
typeLitString = TSTypeLitString

typeLitBoolean ∷ Boolean -> TsTypeLiteral
typeLitBoolean = TSTypeLitBoolean

typeLitArray ∷ Array TsType -> TsTypeLiteral
typeLitArray = TSTypeLitArray

typeLitNumber ∷ Number -> TsTypeLiteral
typeLitNumber = TSTypeLitNumber

typeLitUndefined ∷ TsTypeLiteral
typeLitUndefined = TSTypeLitUndefined

typeLitNull ∷ TsTypeLiteral
typeLitNull = TSTypeLitNull

typeLitObject ∷ Array TsTypeObjectProperty -> TsTypeLiteral
typeLitObject = TSTypeLitObject

typeObjectProperty ∷ TsIdentifier -> TsType -> TsTypeObjectProperty
typeObjectProperty = TsTypeObjectProperty
