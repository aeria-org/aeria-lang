module Aeria.Codegen.Javascript where

import Prelude
import Aeria.Codegen.Javascript.Tree (JSExpression(..))
import Aeria.Syntax.Tree (Attribute(..), Attributes, Collection(..), CollectionName(..), Name(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Value(..))
import Control.Lazy (fix)
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..))
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
  codegenProperty property@( Property
    { propertyName: (PropertyName propertyName)
    }
  ) = propertyName /\ (JSObjectLiteral $ go property)

  codegenAttributes :: Attributes -> L.List (Tuple String JSExpression)
  codegenAttributes = map (\(Attribute (Name name) value) -> name /\ codegenValue value)

  go :: Property -> L.List (Tuple String JSExpression)
  go (Property { propertyName, propertyType, propertyAttributes }) =
    let
      attributes = codegenAttributes propertyAttributes
    in
      case propertyType of
        PEnum ->
          let
            options = L.find (\(Attribute (Name x) _) -> x == "options") propertyAttributes
          in
            case options of
              Just (Attribute (Name _) (VArray value)) -> L.fromFoldable [ "enum" /\ JSArrayLiteral (map codegenValue value) ]
              _ -> L.fromFoldable [ "enum" /\ JSArrayLiteral L.Nil ]
        PFloat -> L.fromFoldable [ "type" /\ JSStringLiteral "number" ] `L.union` attributes
        PInteger -> L.fromFoldable [ "type" /\ JSStringLiteral "integer" ] `L.union` attributes
        PString -> L.fromFoldable [ "type" /\ JSStringLiteral "string" ] `L.union` attributes
        PBoolean -> L.fromFoldable [ "type" /\ JSStringLiteral "boolean" ] `L.union` attributes
        PFile -> L.fromFoldable [ "$ref" /\ JSStringLiteral "file" ] `L.union` attributes
        PArray typ ->
          L.fromFoldable
            [ "type" /\ JSStringLiteral "array"
            , "items"
                /\ ( JSObjectLiteral
                      (go (Property { propertyName, propertyType: typ, propertyAttributes }) `L.union` attributes)
                  )
            ]
        PObject properties' ->
          L.fromFoldable
            [ "type" /\ JSStringLiteral "object"
            , "properties" /\ codegenProperties properties'
            ]
            `L.union`
              attributes
        PCollection (CollectionName collection) -> L.fromFoldable [ "$ref" /\ JSStringLiteral collection ] `L.union` attributes

codegenValue :: Value -> JSExpression
codegenValue =
  fix \self -> case _ of
    (VInteger integer) -> JSNumberLiteral (toNumber integer)
    (VFloat float) -> JSNumberLiteral float
    (VString str) -> JSStringLiteral str
    (VBoolean bool) -> JSBooleanLiteral bool
    (VArray arr) -> JSArrayLiteral (map self arr)
    (VProperty (Name name)) -> JSStringLiteral name
