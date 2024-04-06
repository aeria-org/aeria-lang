module Aeria.Codegen where

import Prelude

import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Type (codegenType)
import Aeria.Codegen.Typescript.Tree as Ts
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Attributes, Collection(..), CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionFunctions, CollectionGetters, CollectionIcon(..), CollectionImmutable(..), CollectionIndexes, CollectionLayout, CollectionName(..), CollectionOwned(..), CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionTable, CollectionTableMeta, CollectionTimestamps(..), CollectionWritable, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), FunctionItem(..), Getter(..), ImmutableItem(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), Macro(..), Program(..), Property(..), PropertyName(..), PropertyType(..), Required(..), TableItem(..), TableMetaItem(..), WritableItem(..))
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
  , owned
  , timestamps
  , search
  , filtersPresets
  , layout
  , writable
  , functions
  , immutable
  }
) = go
  where
  go = Js.object [Js.objectProperty "description" cDescription]

  cDescription = Js.object (description)
    where
    description = concat
      [ baseDescription
      , iconDescription
      , ownedDescription
      , timestampsDescription
      , tableDescription
      , functionsDescription
      , writableDescription
      , tableMetaDescription
      , requiredDescription
      , filtersDescription
      , formDescription
      , indexesDescription
      , searchDescription
      , immutableDescription
      , cFiltersPresetsDescription
      , cLayoutDescription
      ]

    baseDescription =
      [ Js.objectProperty "$id" (cCollectionName name)
      , Js.objectProperty "properties" (cCollectionProperties properties getters)
      ]

    iconDescription = cIcon icon

    ownedDescription = cOwned owned

    timestampsDescription = cTimestamps timestamps

    searchDescription = cSearch search

    immutableDescription = cImmutable immutable

    tableDescription = cConditional cTable "table" table

    writableDescription = cConditional cWritable "writable" writable

    functionsDescription = cConditional cFunctions "functions" functions

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

cImmutable :: Maybe CollectionImmutable -> Array Js.JsObjectProperty
cImmutable Nothing = []
cImmutable (Just (CollectionImmutableBool bool)) = [Js.objectProperty "immutable" (Js.boolean bool)]
cImmutable (Just (CollectionImmutableList immutable)) =
  cConditional cImmutable' "immutable" immutable
  where
    cImmutable' immutable' = cPropertiesList (map (\(ImmutableItem _ propertyName) -> propertyName) immutable')

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

cOwned :: Maybe CollectionOwned -> Array Js.JsObjectProperty
cOwned Nothing = []
cOwned (Just (CollectionOwned bool)) = [Js.objectProperty "owned" (Js.boolean bool)]

cTimestamps :: Maybe CollectionTimestamps -> Array Js.JsObjectProperty
cTimestamps Nothing = []
cTimestamps (Just (CollectionTimestamps timestamps)) = [Js.objectProperty "timestamps" (Js.boolean timestamps)]

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

cWritable :: CollectionWritable -> Js.JsTree
cWritable writable = cPropertiesList (map (\(WritableItem _ propertyName) -> propertyName) writable)

cFunctions :: CollectionFunctions -> Js.JsTree
cFunctions writable = cPropertiesList (map (\(FunctionItem _ propertyName) -> propertyName) writable)

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

cCollectionProperties :: CollectionProperties -> CollectionGetters -> Js.JsTree
cCollectionProperties properties getters = Js.object $ union (cProperties properties) (cGetters getters)
  where
    cGetters :: CollectionGetters -> Array Js.JsObjectProperty
    cGetters getters' = L.toUnfoldable $ map go getters'
      where
        go :: Getter -> Js.JsObjectProperty
        go (Getter { name: (PropertyName _ propertyName), macro}) =
          Js.objectProperty propertyName (cGetter macro)

        cGetter :: Macro -> Js.JsTree
        cGetter (Macro _ code) = Js.object
          [ Js.objectProperty "getter" (Js.arrowFunction [Js.identifier "doc"] (Js.code code))
          ]

    cProperties :: CollectionProperties -> Array Js.JsObjectProperty
    cProperties properties' = L.toUnfoldable $ map go properties'
      where
        go :: Property -> Js.JsObjectProperty
        go property@(Property { name: (PropertyName _ propertyName)})
          = Js.objectProperty propertyName (cProperty property)

        cProperty :: Property -> Js.JsTree
        cProperty property@(Property { type_, attributes }) =
          Js.object $ concat [props, attrs]
          where
            attrs = cAttributes attributes
            props =
              case type_ of
                PFloat _ ->  [cType "number"]
                PInteger _ ->  [cType "integer"]
                PString _ ->  [cType "string"]
                PBoolean _ ->  [cType "boolean"]
                PEnum _ ->
                  [ Js.objectProperty "enum" (cEnumType property)
                  ]
                PArray _ type_' ->
                  [ cType "array"
                  , cArrayType property type_'
                  ]
                PObject _  properties'' ->
                  [ cType "object"
                  , Js.objectProperty "properties" (Js.object $ cProperties properties'')
                  ]
                PRef _ collectionName ->
                  [ Js.objectProperty "$ref" (cCollectionName collectionName)
                  ]

            cType type_' = Js.objectProperty "type" (Js.string type_')

        cAttributes :: Attributes -> Array Js.JsObjectProperty
        cAttributes attributes = L.toUnfoldable $ map go' attributes
          where
            go' (Attribute _ (AttributeName _ attributeName) attributeValue) =
              case attributeValue of
                ALiteral _ literal -> Js.objectProperty attributeName (cLiteral literal)
                AExpr _ expr -> Js.objectProperty attributeName (cExpr expr)

        cEnumType (Property { attributes }) = Js.array cOptions
          where
          cOptions =
            case L.find (\(Attribute _ (AttributeName _ x) _) -> x == "options") attributes of
              Just (Attribute _ _ (ALiteral _ (LArray _ value))) ->
                L.toUnfoldable $ map cLiteral value
              _ -> []

        cArrayType (Property { span, name, attributes }) type_' =
          Js.objectProperty "items" $ Js.object
            [go (Property { span, name, type_: type_', attributes })]

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
