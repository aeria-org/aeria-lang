module Aeria.Codegen.Javascript.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)

data TargetModule
  = CommonJs
  | EsNext

data Identifier = Identifier String

derive instance genericIdentifier :: Generic Identifier _

instance eqIdentifier :: Eq Identifier where
  eq x = genericEq x

data Literal
  = Null -- null
  | Undefined -- undefined
  | String String -- "a", "ab"
  | Number Number -- 1, 2.0, 3
  | Boolean Boolean -- true, false
  | Array (Array Tree) -- [1, 2, 3]

derive instance genericLiteral :: Generic Literal _

instance eqLiteral :: Eq Literal where
  eq = genericEq

data ObjectProperty
  = ObjectProperty1 Identifier -- { x }
  | ObjectProperty2 Identifier Tree -- {x: y}

derive instance genericObjectProperty :: Generic ObjectProperty _

instance eqObjectProperty :: Eq ObjectProperty where
  eq = genericEq

data ImportSpecifier
  = ImportSpecifier Identifier -- { x }
  | ImportAliasSpecifier Identifier Identifier -- { x as y }

derive instance genericImportSpecifier :: Generic ImportSpecifier _

instance eqImportSpecifier :: Eq ImportSpecifier where
  eq = genericEq

data Specifiers = Specifiers (Array ImportSpecifier) -- { x, y, z as z1 }

derive instance genericSpecifiers :: Generic Specifiers _

instance eqSpecifiers :: Eq Specifiers where
  eq = genericEq

data Tree
  = Literal Literal -- 1, "a", true
  | Variable Identifier -- a, b, abc
  | Call Tree (Array Tree) -- a(), b(argm1, argm2, argm3)
  | Function (Array Identifier) Tree -- (a, b, ...) => a
  | Object (Array ObjectProperty) -- { x: 1, y: "a", z: false }
  | Raw String -- "console.log('hello')"

derive instance genericTree :: Generic Tree _

instance eqTree :: Eq Tree where
  eq x = genericEq x

data Statement
  = ImportDeclaration Specifiers Identifier
  | VariableDeclaration Identifier Tree
  | ExportDeclaration Statement
  | EmptyStatement

derive instance genericStatement :: Generic Statement _

instance eqStatement :: Eq Statement where
  eq x = genericEq x

data Statements = Statements (Array Statement)

derive instance genericStatements :: Generic Statements _

instance eqStatements :: Eq Statements where
  eq = genericEq

objectProperty2 :: String -> Tree -> ObjectProperty
objectProperty2 ident tree = ObjectProperty2 (identifier ident) tree

objectProperty1 :: String -> ObjectProperty
objectProperty1 ident = ObjectProperty1 (identifier ident)

object :: Array ObjectProperty -> Tree
object o = Object o

array :: Array Tree -> Tree
array a = Literal (Array a)

string :: String -> Tree
string s = Literal (String s)

boolean :: Boolean -> Tree
boolean b = Literal (Boolean b)

float :: Number -> Tree
float f = Literal (Number f)

int :: Int -> Tree
int i = Literal (Number (toNumber i))

undefined :: Tree
undefined = Literal Undefined

null :: Tree
null = Literal Null

identifier :: String -> Identifier
identifier s = Identifier s

raw :: String -> Tree
raw = Raw

function :: Array Identifier -> Tree -> Tree
function = Function

variable :: String -> Tree
variable = Variable <<< Identifier

call :: Tree -> Array Tree -> Tree
call = Call

importDeclaration ∷ String -> Specifiers -> Statement
importDeclaration i s = ImportDeclaration s (Identifier i)

variableDeclaration ∷ String -> Tree -> Statement
variableDeclaration n = VariableDeclaration (Identifier n)

exportDeclaration ∷ Statement -> Statement
exportDeclaration = ExportDeclaration

emptyStatement ∷ Statement
emptyStatement = EmptyStatement

specifiers :: Array ImportSpecifier -> Specifiers
specifiers = Specifiers

importSpecifier1 :: String -> ImportSpecifier
importSpecifier1 x = ImportSpecifier (Identifier x)

importSpecifier2 :: String -> String -> ImportSpecifier
importSpecifier2 x y = ImportAliasSpecifier (Identifier x) (Identifier y)

statements ∷ Array Statement -> Statements
statements = Statements
