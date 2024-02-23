module Aeria.Codegen.Javascript where

import Prelude
import Aeria.Codegen.Javascript.Tree (JsTree(..))
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), Collection(..), CollectionName(..), Expr(..), Getter(..), Getters, Literal(..), Macro(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Required, RequiredProperty(..), Table)
import Control.Lazy (fix)
import Data.Array (union)
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))

codegen :: Program -> L.List (Tuple String JsTree)
codegen (Program { collections }) = map go collections
  where
  go collection@(Collection { name: (CollectionName name) }) =
    name
      /\ ( JSStatments
            $ L.fromFoldable
                [ JSImport (JSDestructuringObject $ L.fromFoldable [ JSIdentifier "defineCollection", JSIdentifier "deepMerge" ]) "sonata-api"
                , JSExport name
                    ( JSArrow (L.fromFoldable [ "description" ])
                        $ JSCall "defineCollection"
                        $ L.fromFoldable
                            [ JSCall "deepMerge" (L.fromFoldable [ codegenCollection collection, JSIdentifier "description" ])
                            ]
                    )
                ]
        )

codegenCollection :: Collection -> JsTree
codegenCollection ( Collection
    { name: (CollectionName collectionName)
  , required
  , properties
  , getters
  , table
  }
) =
  JSObjectLiteral
    $ L.fromFoldable
        [ "description" /\ codegenDescription
        ]
  where
  codegenDescription = JSObjectLiteral (L.fromFoldable description)
    where
    description =
      let
        description' = union baseDescription tableDescription
      in
        union description' requiredDescription

    baseDescription =
      [ "$id" /\ JSStringLiteral collectionName
      , "properties" /\ codegenProperties properties getters
      ]

    tableDescription = case table of
      L.Nil -> []
      _ -> [ "table" /\ codegenTable table ]

    requiredDescription = case required of
      L.Nil -> []
      _ -> [ "required" /\ codegenRequired required ]

codegenTable :: Table -> JsTree
codegenTable table = JSArrayLiteral (map (\(PropertyName name) -> JSStringLiteral name) table)

codegenGetters :: Getters -> L.List (Tuple String JsTree)
codegenGetters getters = map go getters
  where
  go (Getter { name: (PropertyName name), macro: (Macro _ code) }) =
    name
      /\ ( JSObjectLiteral
            $ L.fromFoldable
                [ "getter" /\ JSCode code
                ]
        )

codegenRequired :: Required -> JsTree
codegenRequired required =
  if hasCondition then
    JSArrayLiteral $ map (\(RequiredProperty (PropertyName name) _) -> JSStringLiteral name) required
  else
    JSObjectLiteral $ map (\(RequiredProperty (PropertyName name) expr) -> name /\ (codegenObject expr)) required
  where
  hasCondition = L.all (\(RequiredProperty _ cond) -> isNothing cond) required

  codegenObject Nothing = JSBooleanLiteral true

  codegenObject (Just cond) = codegenExpr cond

codegenBinaryExpr :: String -> JsTree -> JsTree -> JsTree
codegenBinaryExpr oper e1 e2 =
  JSObjectLiteral
    $ L.fromFoldable
        [ "operator" /\ JSStringLiteral oper
        , "term1" /\ e1
        , "term2" /\ e2
        ]

codegenUnaryExpr :: String -> JsTree -> JsTree
codegenUnaryExpr oper e1 =
  JSObjectLiteral
    $ L.fromFoldable
        [ "operator" /\ JSStringLiteral oper
        , "term1" /\ e1
        ]

codegenExpr :: Expr -> JsTree
codegenExpr (ELiteral value) = codegenLiteral value

codegenExpr (EExists e1) = codegenUnaryExpr "exists" (codegenExpr e1)

codegenExpr (ENot e1) = codegenUnaryExpr "not" (codegenExpr e1)

codegenExpr (EOr e1 e2) =
  JSObjectLiteral
    $ L.fromFoldable
        [ "or" /\ JSArrayLiteral (codegenExpr e1 L.: codegenExpr e2 L.: L.Nil)
        ]

codegenExpr (EAnd e1 e2) =
  JSObjectLiteral
    $ L.fromFoldable
        [ "and" /\ JSArrayLiteral (codegenExpr e1 L.: codegenExpr e2 L.: L.Nil)
        ]

codegenExpr (EIn e1 e2) = codegenBinaryExpr "in" (codegenExpr e1) (codegenExpr e2)

codegenExpr (ELt e1 e2) = codegenBinaryExpr "lt" (codegenExpr e1) (codegenExpr e2)

codegenExpr (EGt e1 e2) = codegenBinaryExpr "gt" (codegenExpr e1) (codegenExpr e2)

codegenExpr (ELte e1 e2) = codegenBinaryExpr "lte" (codegenExpr e1) (codegenExpr e2)

codegenExpr (EGte e1 e2) = codegenBinaryExpr "gte" (codegenExpr e1) (codegenExpr e2)

codegenExpr (EEq e1 e2) = codegenBinaryExpr "eq" (codegenExpr e1) (codegenExpr e2)

codegenProperties :: Properties -> Getters -> JsTree
codegenProperties properties getters = JSObjectLiteral (properties' `L.union` getters')
  where
  codegenProperty :: Property -> (Tuple String JsTree)
  codegenProperty property@( Property
      { name: (PropertyName propertyName)
    }
  ) = propertyName /\ (JSObjectLiteral $ go property)

  properties' = map codegenProperty properties

  getters' = codegenGetters getters

  codegenAttributes = map (\(Attribute (AttributeName name) value) -> name /\ codegenLiteral value)

  go :: Property -> L.List (Tuple String JsTree)
  go (Property { name, type_, attributes }) =
    let
      attributes' = codegenAttributes attributes
    in
      case type_ of
        PEnum ->
          let
            options = L.find (\(Attribute (AttributeName x) _) -> x == "options") attributes
          in
            case options of
              Just (Attribute (AttributeName _) (LArray value)) -> L.fromFoldable [ "enum" /\ JSArrayLiteral (map codegenLiteral value) ]
              _ -> L.fromFoldable [ "enum" /\ JSArrayLiteral L.Nil ]
        PFloat -> L.fromFoldable [ "type" /\ JSStringLiteral "number" ] `L.union` attributes'
        PInteger -> L.fromFoldable [ "type" /\ JSStringLiteral "integer" ] `L.union` attributes'
        PString -> L.fromFoldable [ "type" /\ JSStringLiteral "string" ] `L.union` attributes'
        PBoolean -> L.fromFoldable [ "type" /\ JSStringLiteral "boolean" ] `L.union` attributes'
        PArray type_' ->
          L.fromFoldable
            [ "type" /\ JSStringLiteral "array"
            , "items"
                /\ ( JSObjectLiteral
                      (go (Property { name, type_: type_', attributes }) `L.union` attributes')
                  )
            ]
        PObject properties'' ->
          L.fromFoldable
            [ "type" /\ JSStringLiteral "object"
            , "properties" /\ codegenProperties properties'' L.Nil
            ]
            `L.union`
              attributes'
        PRef (CollectionName collection) -> L.fromFoldable [ "$ref" /\ JSStringLiteral collection ] `L.union` attributes'

codegenLiteral :: Literal -> JsTree
codegenLiteral =
  fix \self -> case _ of
    (LInteger integer) -> JSNumberLiteral (toNumber integer)
    (LFloat float) -> JSNumberLiteral float
    (LString str) -> JSStringLiteral str
    (LBoolean bool) -> JSBooleanLiteral bool
    (LArray arr) -> JSArrayLiteral (map self arr)
    (LProperty (PropertyName name)) -> JSStringLiteral name
