module Aeria.Codegen.Type
  ( codegenType
  )
  where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsIdentifier(..), JsLiteral(..), JsObjectProperty(..), JsTree(..))
import Aeria.Codegen.Typescript.Tree (TsIdentifier(..), TsParameter(..), TsType(..), TsTypeLiteral(..), TsTypeObjectProperty(..))
import Data.List as L

codegenType :: JsTree -> TsType
codegenType =
  case _ of
    JSCode _code -> TSTypeString
    JSIdentifier ident -> TSTypeReference L.Nil (codegenIdent ident)
    JSLiteral literal ->  codegenLiteral literal
    JSCallExpression _called _params ->
      TSTypeAny
    JSArrowFunctionExpression params body ->
      TSFunctionType L.Nil (map codegenParameter params) (codegenType body)

codegenLiteral :: JsLiteral -> TsType
codegenLiteral literal = TSTypeLiteral (go literal)
  where
    go = case _ of
      JSString value -> TSTypeLitString value
      JSNumber value -> TSTypeLitNumber value
      JSBoolean value -> TSTypeLitBoolean value
      JSArray value -> TSTypeLitArray (map codegenType value)
      JSObject value -> TSTypeLitObject (map codegenObjectProperty value)

codegenObjectProperty :: JsObjectProperty -> TsTypeObjectProperty
codegenObjectProperty (JsObjectProperty ident tree) =
  TsTypeObjectProperty (codegenIdent ident) (codegenType tree)

codegenParameter :: JsIdentifier -> TsParameter
codegenParameter ident = TsParameter (codegenIdent ident) TSTypeAny

codegenIdent :: JsIdentifier -> TsIdentifier
codegenIdent (JsIdentifier ident) = TsIdentifier ident
