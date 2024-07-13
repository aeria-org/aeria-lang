module Aeria.Codegen.Type
  ( typegen
  ) where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsLiteral(..), JsObjectProperty(..), JsTree(..))
import Aeria.Codegen.Typescript.Tree as Ts
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

typegen :: JsTree -> Ts.TsType
typegen = case _ of
  JSCode _ -> Ts.typeAny
  JSIdentifier ident -> Ts.typeReference [] (identTypeGen (JSIdentifier ident))
  JSLiteral literal -> literalTypeGen literal
  JSCallExpression _ _ -> Ts.typeAny
  JSArrowFunctionExpression params body ->
    Ts.functionType [] (map parameterTypeGen params) (typegen body)

literalTypeGen :: JsLiteral -> Ts.TsType
literalTypeGen = Ts.typeLiteral <<< case _ of
  JSNull -> Ts.typeLitNull
  JSUndefined -> Ts.typeLitUndefined
  JSString value -> Ts.typeLitString value
  JSNumber value -> Ts.typeLitNumber value
  JSBoolean value -> Ts.typeLitBoolean value
  JSArray value -> Ts.typeLitArray (map typegen value)
  JSObject value -> Ts.typeLitObject (map objectPropertyTypeGen value)

objectPropertyTypeGen :: JsObjectProperty -> Ts.TsTypeObjectProperty
objectPropertyTypeGen = case _ of
  JsObjectProperty1 ident ->
    Ts.typeObjectProperty (identTypeGen ident) (Ts.typeQuery (identTypeGen ident))
  JsObjectProperty2 ident tree ->
    Ts.typeObjectProperty (identTypeGen ident) (typegen tree)

parameterTypeGen :: JsTree -> Ts.TsParameter
parameterTypeGen ident = Ts.parameter (identTypeGen ident) Ts.typeAny

identTypeGen :: JsTree -> Ts.TsIdentifier
identTypeGen = case _ of
  JSIdentifier ident -> Ts.identifier ident
  _ -> unsafePerformEffect (throw "Unexpected JsTree value in identTypeGen")
