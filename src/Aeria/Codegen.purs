module Aeria.Codegen where

import Prelude

import Aeria.Codegen.Type (codegenType)
import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Typescript.Tree as Ts
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionGetters, CollectionIcon(..), CollectionIndexes, CollectionLayout, CollectionName(..), CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionTable, CollectionTableMeta, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), Getter(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), Macro(..), Program(..), Property(..), PropertyName(..), PropertyType(..), Required(..), TableItem(..), TableMetaItem(..))
import Control.Lazy (fix)
import Data.Array (concat, union)
import Data.List as L
import Data.Maybe (Maybe(..), isNothing)
import Data.String.Utils (ucfirst)

data Codegen
  = Codegen String Js.JsStatements Ts.TsStatements

codegen :: Program -> L.List Codegen
codegen (Program { collections }) = map go collections
  where
  go collection@(Collection { name: (CollectionName _ collectionName) }) =
    Codegen collectionName jsFile tsFile
    where
    collection' = cCollection collection

    tsFile =
      Ts.statements
        [ Ts.import_
            (Ts.specifiers
              [ Ts.importSpecifier (Ts.identifier "Collection")
              , Ts.importSpecifier (Ts.identifier "SchemaWithId")
              ])
            (Ts.identifier "aeria")
        , Ts.exportNamed
            (Ts.variable
              [Ts.declareKeyword, Ts.constKeyword]
              (Ts.identifier collectionName)
              (codegenType collection'))
        , Ts.exportNamed
          (Ts.typeAlias
            (Ts.identifier (ucfirst collectionName))
            ( Ts.typeReference
              [Ts.typeParameter $ Ts.typeQuery (Ts.identifier (collectionName <> ".description"))]
              (Ts.identifier "SchemaWithId")
            )
          )
        , Ts.exportNamed
            (Ts.typeAlias
              (Ts.identifier "extendCollection")
              (Ts.functionType
                [ Ts.typeParameter
                    (Ts.typeExtends
                      (Ts.typeReference [] (Ts.identifier "T"))
                      (Ts.typeReference [] (Ts.identifier "Collection")))]

                [Ts.parameter
                  (Ts.identifier "collection")
                  (Ts.typeReference [] (Ts.identifier "T")) ]
                  (Ts.intersectionType
                    (Ts.typeReference [] (Ts.identifier "T"))
                    (Ts.typeQuery (Ts.identifier collectionName)))))]

    jsFile =
      Js.statements
        [ Js.import_
            (Js.specifiers
              [ Js.importSpecifier (Js.identifier "defineCollection")
              , Js.importSpecifier (Js.identifier "deepMerge")
              ])
            (Js.identifier "aeria")
        , Js.exportNamed (Js.variable (Js.identifier collectionName) collection')
        , Js.exportNamed
            (Js.variable
              (Js.identifier "extendCollection")
              (Js.arrowFunction
                [Js.identifier "collection"]
                (Js.call (Js.identifier "defineCollection")
                  [ Js.call (Js.identifier "deepMerge")
                    [Js.identifier collectionName, Js.identifier "collection"]
                  ]
                )))]

cCollection :: Collection -> Js.JsTree
cCollection ( Collection
  { name
  , required
  , properties
  , getters
  , table
  , tableMeta
  , form
  , filters
  , indexes
  , icon
  , search
  , filtersPresets
  , layout
  }
) = go
  where
  go = Js.object [Js.objectProperty "description" cDescription]

  cDescription = Js.object (description)
    where
    description = concat
      [ baseDescription
      , iconDescription
      , tableDescription
      , tableMetaDescription
      , requiredDescription
      , filtersDescription
      , formDescription
      , indexesDescription
      , searchDescription
      , cFiltersPresetsDescription
      , cLayoutDescription
      ]

    baseDescription =
      [ Js.objectProperty "$id" (cCollectionName name)
      , Js.objectProperty "properties" (cProperties properties getters)
      ]

    iconDescription = cIcon icon

    searchDescription = cSearch search

    tableDescription = cConditional cTable "table" table

    requiredDescription = cConditional cRequired "required" required

    tableMetaDescription = cConditional cTableMeta "tableMeta" tableMeta

    formDescription = cConditional cForm "form" form

    filtersDescription = cConditional cFilters "filters" filters

    indexesDescription = cConditional cIndexes "indexes" indexes

    cFiltersPresetsDescription = cConditional cFiltersPresets "filtersPresets" filtersPresets

    cLayoutDescription = cConditional cLayout "layout" layout


    cConditional :: forall a. (L.List a -> Js.JsTree) -> String -> L.List a -> Array Js.JsObjectProperty
    cConditional f key value = case value of
      L.Nil -> []
      _ -> [Js.objectProperty key (f value)]

cLayout :: CollectionLayout -> Js.JsTree
cLayout layout = Js.object $ L.toUnfoldable $ map go layout
  where
    go (LayoutItem { name: (PropertyName _ name), component, span_, if_, verticalSpacing  }) =
      Js.objectProperty name (
        Js.object $ concat
          [ cMaybe (\label' -> [Js.objectProperty "span" (Js.float label')]) span_ []
          , cMaybe (\label' -> [Js.objectProperty "verticalSpacing" (Js.float label')]) verticalSpacing []
          , cMaybe (\(Cond _ if_') -> [Js.objectProperty "if" (cExpr if_')]) if_ []
          , cMaybe (\component' -> [Js.objectProperty "component" (cComponent component')]) component []
          ]
      )

    cComponent (LayoutItemComponent {name, props}) = Js.object
      $ concat
        [ cMaybe (\name' -> [Js.objectProperty "name" (Js.string name')]) name []
        , cMaybe (\(Macro _ props') -> [Js.objectProperty "props" (Js.code props')]) props []
        ]

cIcon :: Maybe CollectionIcon -> Array Js.JsObjectProperty
cIcon Nothing = []
cIcon (Just (CollectionIcon icon)) = [Js.objectProperty "icon" (Js.string icon)]

cSearch :: Maybe CollectionSearch -> Array Js.JsObjectProperty
cSearch Nothing = []
cSearch (Just (CollectionSearch { placeholder, indexes }))
  = [ Js.objectProperty "search"
        $ Js.object $
          concat
            [ [ Js.objectProperty "indexes" (cPropertiesList indexes)
              ]
            , cMaybe (\placeholder' -> [Js.objectProperty "placeholder" (Js.string placeholder')]) placeholder []
            ]
    ]

cPropertiesList :: L.List PropertyName -> Js.JsTree
cPropertiesList list = Js.array (L.toUnfoldable $ map cPropertyName list)

cFiltersPresets :: CollectionFiltersPresets -> Js.JsTree
cFiltersPresets filtersPresets = Js.object $ L.toUnfoldable $ map go filtersPresets
  where
    go (FiltersPresetsItem { name: (PropertyName _ name), label, badgeFunction, filters  }) =
      Js.objectProperty name (
        Js.object $ concat
          [ cMaybe (\label' -> [Js.objectProperty "name" (Js.string label')]) label []
          , cMaybe (\badgeFunction' -> [Js.objectProperty "badgeFunction" (Js.string badgeFunction')]) badgeFunction []
          , cMaybe (\(Macro _ code') -> [Js.objectProperty "filters" (Js.code code')]) filters []
          ]
      )
cTable :: CollectionTable -> Js.JsTree
cTable table = cPropertiesList (map (\(TableItem _ propertyName) -> propertyName) table)

cTableMeta :: CollectionTableMeta -> Js.JsTree
cTableMeta tableMeta = cPropertiesList (map (\(TableMetaItem _ propertyName) -> propertyName) tableMeta)

cForm :: CollectionForm -> Js.JsTree
cForm form = cPropertiesList (map (\(FormItem _ propertyName) -> propertyName) form)

cFilters :: CollectionFilters -> Js.JsTree
cFilters filters = cPropertiesList (map (\(FilterItem _ propertyName) -> propertyName) filters)

cIndexes :: CollectionIndexes -> Js.JsTree
cIndexes indexes = cPropertiesList (map (\(IndexesItem _ propertyName) -> propertyName) indexes)

cGetters :: CollectionGetters -> Array Js.JsObjectProperty
cGetters getters = L.toUnfoldable $ map go getters
  where
  go (Getter { name: (PropertyName _ propertyName), macro: (Macro _ code) }) = Js.objectProperty propertyName value
    where
    value = Js.object
      [Js.objectProperty "getter" (Js.arrowFunction [Js.identifier "doc"] (Js.code code))]

cRequired :: CollectionRequired -> Js.JsTree
cRequired required =
  if hasCondition then
     Js.array $ L.toUnfoldable $ map (\(Required _ propertyName _) -> cPropertyName propertyName) required
  else
    Js.object
      $ L.toUnfoldable $ map
          ( \(Required _ (PropertyName _ propertyName) expr) ->
              Js.objectProperty propertyName (cObject expr)
          )
          required
  where
  hasCondition = L.all (\(Required _ _ cond) -> isNothing cond) required

  cObject Nothing = Js.boolean true

  cObject (Just (Cond _ cond)) = cExpr cond

cBinaryExpr :: String -> Js.JsTree -> Js.JsTree -> Js.JsTree
cBinaryExpr oper e1 e2 =
  Js.object
    [ Js.objectProperty "operator" (Js.string oper)
    , Js.objectProperty "term1" e1
    , Js.objectProperty "term2" e2
    ]

cUnaryExpr :: String -> Js.JsTree -> Js.JsTree
cUnaryExpr oper e1 =
  Js.object
    [ Js.objectProperty "operator" (Js.string oper)
    , Js.objectProperty "term1" e1
    ]

cExpr :: Expr -> Js.JsTree
cExpr (ELiteral value) = cLiteral value
cExpr (EExists e1) = cUnaryExpr "exists" (cExpr e1)
cExpr (ENot e1) = cUnaryExpr "not" (cExpr e1)
cExpr (EOr e1 e2) =
  Js.object
    [ Js.objectProperty "or" (Js.array [cExpr e1, cExpr e2])
    ]
cExpr (EAnd e1 e2) =
  Js.object
    [ Js.objectProperty "and" (Js.array [cExpr e1, cExpr e2])
    ]
cExpr (EIn e1 e2) = cBinaryExpr "in" (cExpr e1) (cExpr e2)
cExpr (ELt e1 e2) = cBinaryExpr "lt" (cExpr e1) (cExpr e2)
cExpr (EGt e1 e2) = cBinaryExpr "gt" (cExpr e1) (cExpr e2)
cExpr (ELte e1 e2) = cBinaryExpr "lte" (cExpr e1) (cExpr e2)
cExpr (EGte e1 e2) = cBinaryExpr "gte" (cExpr e1) (cExpr e2)
cExpr (EEq e1 e2) = cBinaryExpr "eq" (cExpr e1) (cExpr e2)

cProperties :: CollectionProperties -> CollectionGetters -> Js.JsTree
cProperties properties getters = Js.object (union properties' getters')
  where
  cProperty :: Property -> Js.JsObjectProperty
  cProperty property@( Property
    { name: (PropertyName _ propertyName)
    }
  ) = Js.objectProperty propertyName (Js.object $ go property)

  properties' = L.toUnfoldable $ map cProperty properties

  getters' = cGetters getters

  cAttributes attributes =
    L.toUnfoldable $ map
      ( \(Attribute _ (AttributeName _ attributeName) value) ->
        case value of
          ALiteral _ value' -> Js.objectProperty attributeName (cLiteral value')
          AExpr _ expr -> Js.objectProperty attributeName (cExpr expr)
      ) attributes

  go :: Property -> Array Js.JsObjectProperty
  go property@(Property { span, name, type_, attributes }) =
    let
      attributes' = cAttributes attributes
    in
      case type_ of
        PEnum _ -> cEnum property
        PFloat _ -> union attributes' [Js.objectProperty "type" (Js.string "number") ]
        PInteger _ -> union attributes' [Js.objectProperty "type" (Js.string "integer") ]
        PString _ -> union attributes' [Js.objectProperty "type" (Js.string "string") ]
        PBoolean _ -> union attributes' [Js.objectProperty "type" (Js.string "boolean") ]
        PArray _ type_' -> union attributes' $ cArray property type_'
        PObject _ properties'' ->
          union attributes'
            [ Js.objectProperty "type" (Js.string "object")
            , Js.objectProperty "properties" (cProperties properties'' L.Nil)
            ]
        PRef _ collectionName ->
          union attributes'
            [ Js.objectProperty "$ref" (cCollectionName collectionName)
            ]

  cArray (Property { span, name, attributes }) type_' =
      [ Js.objectProperty "items" (Js.object (go (Property { span, name, type_: type_', attributes })))
      , Js.objectProperty "type" (Js.string "array")
      ]

  cEnum (Property {attributes}) =
    case options of
      Just (Attribute _ (AttributeName _ _) (ALiteral _ (LArray _ value))) ->
        [Js.objectProperty "enum" (Js.array (L.toUnfoldable $ map cLiteral value))]
      _ -> [Js.objectProperty "enum" (Js.array []) ]
    where
    options = L.find (\(Attribute _ (AttributeName _ x) _) -> x == "options") attributes

cLiteral :: Literal -> Js.JsTree
cLiteral =
  fix \self -> case _ of
    LInteger _ i -> Js.int i
    LFloat _ f -> Js.float f
    LString _ s -> Js.string s
    LBoolean _ b -> Js.boolean b
    LArray _ a -> Js.array (L.toUnfoldable $ map self a)
    LProperty _ propertyName -> cPropertyName propertyName

cCollectionName :: CollectionName -> Js.JsTree
cCollectionName (CollectionName _ collectionName) = Js.string collectionName

cPropertyName :: PropertyName -> Js.JsTree
cPropertyName (PropertyName _ propertyName) = Js.string propertyName

cMaybe :: forall a b. (a -> b) -> Maybe a -> b -> b
cMaybe f x d =
  case x of
    Just a -> f a
    Nothing -> d
