module Aeria.Codegen.Typescript.Tree where

import Data.Tuple (Tuple)

data Identifier = Identifier String

data TypeObjectProperty = TypeObjectProperty Identifier Tree

data TsType
  = TypeNumber -- number
  | TypeString -- string
  | TypeBool -- boolean
  | TypeAny -- any
  | TypeRaw String -- "A extends B<C>"
  | TypeArray TsType -- array
  | TypeLiteralNull -- null
  | TypeLiteralUndefined -- undefined
  | TypeLiteralString String -- "xyz..."
  | TypeLiteralBool Boolean -- true | false
  | TypeLiteralNumber Number -- 1.0, 2.0, 3.0, ...
  | TypeLiteralArray (Array Tree) -- [1, 2, 3, ...]
  | TypeVariable Identifier -- T
  | TypeGeneric (Array Tree) Tree -- A<number, string, B>, <A>(params: A) => A, ...
  | TypeFunction (Array (Tuple Identifier Tree)) Tree -- (x: number, y: string, ...) => number
  | TypeObject (Array TypeObjectProperty) -- { x: number, y: string }

data ImportSpecifier
  = ImportSpecifier Identifier -- { x }
  | ImportAliasSpecifier Identifier Identifier -- { x as y }

data Specifiers
  = Specifiers (Array ImportSpecifier) -- { x, y, z as z1 }

data Tree
  = Type TsType -- number | string | boolean | any | ...
  | Call Identifier (Array Tree) -- x(argm1, argm2, argm3, ...)
  | Variable Identifier -- x
  | TypeQuery Tree -- typeof x
  | Extends Tree Tree -- A extends number
  | Intersection Tree Tree -- { x: number } & { y: string }

data TsStatement
  = VariableDeclaration Identifier Tree -- const x: number
  | TypeAliasDeclaration Identifier Tree -- type x = number
  | ImportDeclaration Specifiers Identifier -- import { x, y } from 'z'
  | ExportDeclaration TsStatement -- export ...
  | DeclareDeclaration TsStatement -- declare ...
  | TSEmptyStatement

data TsStatements = TsStatements (Array TsStatement)

statements ∷ Array TsStatement -> TsStatements
statements = TsStatements

importDeclaration ∷ Specifiers -> Identifier -> TsStatement
importDeclaration = ImportDeclaration

variableDeclaration ∷ Identifier -> Tree -> TsStatement
variableDeclaration = VariableDeclaration

typeAliasDeclaration ∷ Identifier -> Tree -> TsStatement
typeAliasDeclaration = TypeAliasDeclaration

exportDeclaration ∷ TsStatement -> TsStatement
exportDeclaration = ExportDeclaration

declareDeclaration ∷ TsStatement -> TsStatement
declareDeclaration = DeclareDeclaration

emptyStatement ∷ TsStatement
emptyStatement = TSEmptyStatement

specifiers ∷ Array ImportSpecifier -> Specifiers
specifiers = Specifiers

importSpecifier1 ∷ Identifier -> ImportSpecifier
importSpecifier1 x = ImportSpecifier x

importSpecifier2 ∷ Identifier -> Identifier -> ImportSpecifier
importSpecifier2 x y = ImportAliasSpecifier x y

typeString ∷ TsType
typeString = TypeString

typeNumber ∷ TsType
typeNumber = TypeNumber

typeBool ∷ TsType
typeBool = TypeBool

typeAny ∷ TsType
typeAny = TypeAny

typeArray ∷ TsType -> TsType
typeArray = TypeArray

typeRaw ∷ String -> TsType
typeRaw = TypeRaw

typeVariable ∷ String -> TsType
typeVariable x = TypeVariable (Identifier x)

typeGeneric ∷ Array Tree -> Tree -> TsType
typeGeneric = TypeGeneric

typeLiteralString ∷ String -> TsType
typeLiteralString = TypeLiteralString

typeLiteralBoolean ∷ Boolean -> TsType
typeLiteralBoolean = TypeLiteralBool

typeLiteralArray ∷ Array Tree -> TsType
typeLiteralArray = TypeLiteralArray

typeLiteralNumber ∷ Number -> TsType
typeLiteralNumber = TypeLiteralNumber

typeLiteralUndefined ∷ TsType
typeLiteralUndefined = TypeLiteralUndefined

typeLiteralNull ∷ TsType
typeLiteralNull = TypeLiteralNull

typeFunction ∷ (Array (Tuple Identifier Tree)) -> Tree -> TsType
typeFunction = TypeFunction

typeObject ∷ Array TypeObjectProperty -> TsType
typeObject = TypeObject

typeObjectProperty :: Identifier -> Tree -> TypeObjectProperty
typeObjectProperty = TypeObjectProperty

typeQuery ∷ Tree -> Tree
typeQuery = TypeQuery

type_ :: TsType -> Tree
type_ = Type

extends ∷ Tree -> Tree -> Tree
extends = Extends

intersection ∷ Tree -> Tree -> Tree
intersection = Intersection

call ∷ Identifier -> Array Tree -> Tree
call = Call

variable :: String -> Tree
variable x = Variable (Identifier x)

identifier ∷ String -> Identifier
identifier = Identifier
