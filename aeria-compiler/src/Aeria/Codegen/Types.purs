module Aeria.Codegen.Type
  ( codegenType
  )
  where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsLiteral(..), JsObjectProperty(..), JsTree(..))
import Aeria.Codegen.Typescript.Tree as Ts

codegenType :: JsTree -> Ts.TsType
codegenType =
  case _ of
    JSCode _ -> Ts.typeAny
    JSIdentifier ident -> Ts.typeReference [] (codegenIdent (JSIdentifier ident))
    JSLiteral literal ->  codegenLiteral literal
    JSCallExpression _called _params -> Ts.typeAny
    JSArrowFunctionExpression params body ->
      Ts.functionType [] (map codegenParameter params) (codegenType body)

codegenLiteral :: JsLiteral -> Ts.TsType
codegenLiteral literal = Ts.typeLiteral $
  case literal of
    JSString value -> Ts.typeLitString value
    JSNumber value -> Ts.typeLitNumber value
    JSBoolean value -> Ts.typeLitBoolean value
    JSArray value -> Ts.typeLitArray (map codegenType value)
    JSObject value -> Ts.typeLitObject (map codegenObjectProperty value)

codegenObjectProperty :: JsObjectProperty -> Ts.TsTypeObjectProperty
codegenObjectProperty property =
  case property of
    (JsObjectProperty1 ident) -> Ts.typeObjectProperty (codegenIdent ident) (Ts.typeQuery (codegenIdent ident))
    (JsObjectProperty2 ident tree) -> Ts.typeObjectProperty (codegenIdent ident) (codegenType tree)

codegenParameter :: JsTree -> Ts.TsParameter
codegenParameter ident = Ts.parameter (codegenIdent ident) Ts.typeAny

codegenIdent :: JsTree -> Ts.TsIdentifier
codegenIdent (JSIdentifier ident) = Ts.identifier ident
codegenIdent _ = Ts.identifier "error"
