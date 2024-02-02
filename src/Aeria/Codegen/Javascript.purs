module Aeria.Codegen.Javascript where

import Prelude
import Aeria.Codegen.Javascript.Tree (JSExpression(..))
import Aeria.Syntax.Tree (Attribute(..), Attributes, Collection(..), CollectionName(..), Name(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Value(..))
import Control.Lazy (fix)
import Data.Int (toNumber)
import Data.List as L
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

codegen :: Program -> JSExpression
codegen (Program { collection }) =
  let
    Collection { collectionName: (CollectionName collectionName) } = collection
  in
    JSExports collectionName (codegenCollection collection)

codegenCollection :: Collection -> JSExpression
codegenCollection (Collection { collectionName: (CollectionName collectionName), collectionProperties }) =
  JSObjectLiteral
    $ L.fromFoldable
        [ "description" /\ (codegenDescription collectionProperties)
        ]
  where
  codegenDescription _ =
    JSObjectLiteral
      $ L.fromFoldable
          [ "$id" /\ (JSStringLiteral collectionName)
          , "properties" /\ codegenProperties collectionProperties
          ]

codegenProperties :: Properties -> JSExpression
codegenProperties properties = JSObjectLiteral (map codegenProperty properties)
  where
  codegenProperty :: Property -> (Tuple String JSExpression)
  codegenProperty ( Property
    { propertyName: (PropertyName propertyName)
    , propertyType
    , propertyAttributes
    }
  ) =
    let
      attributes = codegenAttributes propertyAttributes
      typ = codegenPropertyType propertyType
    in
      propertyName /\ (JSObjectLiteral $ typ `L.union` attributes)

  codegenAttributes :: Attributes -> L.List (Tuple String JSExpression)
  codegenAttributes = map (\(Attribute (Name name) value) -> name /\ codegenValue value)

  codegenPropertyType :: PropertyType -> L.List (Tuple String JSExpression)
  codegenPropertyType =
    fix \self -> case _ of
      PEnum -> L.fromFoldable [ "type" /\ JSStringLiteral "string" ]
      PFloat -> L.fromFoldable [ "type" /\ JSStringLiteral "number" ]
      PInteger -> L.fromFoldable [ "type" /\ JSStringLiteral "integer" ]
      PString -> L.fromFoldable [ "type" /\ JSStringLiteral "string" ]
      PBoolean -> L.fromFoldable [ "type" /\ JSStringLiteral "boolean" ]
      PFile -> L.fromFoldable [ "$ref" /\ JSStringLiteral "file" ]
      PArray typ ->
        L.fromFoldable
          [ "type" /\ JSStringLiteral "array"
          , "items" /\ (JSObjectLiteral (self typ))
          ]
      PObject properties' ->
        L.fromFoldable
          [ "type" /\ JSStringLiteral "object"
          , "properties" /\ codegenProperties properties'
          ]
      PCollection (CollectionName collection) -> L.fromFoldable [ "$ref" /\ JSStringLiteral collection ]

codegenValue :: Value -> JSExpression
codegenValue =
  fix \self -> case _ of
    (VInteger integer) -> JSNumberLiteral (toNumber integer)
    (VFloat float) -> JSNumberLiteral float
    (VString str) -> JSStringLiteral str
    (VBoolean bool) -> JSBooleanLiteral bool
    (VArray arr) -> JSArrayLiteral (map self arr)
    (VProperty (Name name)) -> JSStringLiteral name
