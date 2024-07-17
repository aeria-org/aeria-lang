module Aeria.Codegen.Type
  ( typegen
  ) where

import Prelude

import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Typescript.Tree as Ts
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

typegen :: Js.Tree -> Ts.Tree
typegen (Js.Raw _) = Ts.type_ Ts.typeAny
typegen (Js.Call _ _) = Ts.type_ Ts.typeAny
typegen (Js.Variable (Js.Identifier ident)) = Ts.type_ $ Ts.typeVariable ident
typegen (Js.Literal literal) = Ts.type_ $ typegenLiteral literal
typegen (Js.Function params body) = Ts.type_ $ Ts.typeFunction (map parameterTypeGen params) (typegen body)
typegen (Js.Object value) = Ts.type_ $ Ts.typeObject (map objectPropertyTypeGen value)

typegenLiteral :: Js.Literal -> Ts.TsType
typegenLiteral Js.Null = Ts.typeLiteralNull
typegenLiteral Js.Undefined = Ts.typeLiteralUndefined
typegenLiteral (Js.String value) = Ts.typeLiteralString value
typegenLiteral (Js.Number value) = Ts.typeLiteralNumber value
typegenLiteral (Js.Boolean value) = Ts.typeLiteralBoolean value
typegenLiteral (Js.Array value) = Ts.typeLiteralArray (map typegen value)

objectPropertyTypeGen :: Js.ObjectProperty -> Ts.TypeObjectProperty
objectPropertyTypeGen (Js.ObjectProperty1 identifier@(Js.Identifier i)) =
  Ts.typeObjectProperty (typegenIdentifier identifier) (Ts.typeQuery (Ts.variable i))
objectPropertyTypeGen (Js.ObjectProperty2 ident tree) =
  Ts.typeObjectProperty (typegenIdentifier ident) (typegen tree)

parameterTypeGen :: Js.Identifier -> Tuple Ts.Identifier Ts.Tree
parameterTypeGen ident = typegenIdentifier ident /\ Ts.type_ Ts.typeAny

typegenIdentifier :: Js.Identifier -> Ts.Identifier
typegenIdentifier (Js.Identifier ident) = Ts.identifier ident
