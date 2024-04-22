module Aeria.Codegen where

import Prelude

import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Type (codegenType)
import Aeria.Codegen.Typescript.Tree as Ts
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Attributes, Collection(..), CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionFunctions, CollectionGetters, CollectionIcon(..), CollectionImmutable(..), CollectionIndexes, CollectionLayout, CollectionName(..), CollectionOwned(..), CollectionPresets, CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionSecurity, CollectionTable, CollectionTableMeta, CollectionTemporary(..), CollectionTimestamps(..), CollectionWritable, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), FunctionItem(..), Getter(..), ImmutableItem(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), Macro(..), PresetItem(..), Program(..), Property(..), PropertyName(..), PropertyType(..), Required(..), SecurityItem(..), SecurityLogging(..), SecurityRateLimiting(..), TableItem(..), TableMetaItem(..), WritableItem(..))
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
  go collection@(Collection { name: (CollectionName _ collectionName), functions }) =
    Codegen collectionName jsFile tsFile
    where
    collection' = cCollection collection

    functionNames = L.toUnfoldable $
      functions
        # L.filter (\(FunctionItem _ _ custom) -> not custom)
        # map functionName

    tsFile =
      Ts.statements
        [ Ts.import_
            (Ts.specifiers
              $ concat
                [ [ Ts.importSpecifier (Ts.identifier "Collection")
                  , Ts.importSpecifier (Ts.identifier "SchemaWithId")
                  , Ts.importSpecifier (Ts.identifier "ExtendCollection")
                  ]
                , map (Ts.importSpecifier <<< Ts.identifier) functionNames
                ]
              )
            (Ts.identifier "aeria")

        , Ts.exportNamed
            (Ts.typeAlias
              -- [Ts.declareKeyword, Ts.constKeyword]
              (Ts.identifier (collectionName <> "Collection"))
              (codegenType collection'))

        , Ts.exportNamed
            (Ts.variable
              [Ts.declareKeyword, Ts.constKeyword]
              (Ts.identifier collectionName)
              (Ts.intersectionType
                (Ts.typeReference [] (Ts.identifier (collectionName <> "Collection")))
                (Ts.typeLiteral
                  (Ts.typeLitObject
                    [Ts.typeObjectProperty
                      (Ts.identifier "item")
                      (Ts.typeReference
                        [ Ts.typeParameter (Ts.typeReference [] (Ts.identifier (collectionName <> "Collection['description']")))
                        ]
                        (Ts.identifier "SchemaWithId"))]))))

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
                      (Ts.typeReference [] (Ts.identifier "const TCollection"))
                      (Ts.typeReference [] (Ts.identifier "{ [P in keyof Collection]?: Partial<Collection[P]> }"))

                      )] --

                [Ts.parameter
                  (Ts.identifier "collection")
                  (Ts.typeReference [] (Ts.identifier "TCollection")) ]
                  (Ts.typeReference
                    [ Ts.typeParameter $ Ts.typeQuery (Ts.identifier collectionName)
                    , Ts.typeParameter $ Ts.typeReference [] (Ts.identifier "TCollection")
                    ]
                    (Ts.identifier "ExtendCollection"))))]

    jsFile =
      Js.statements
        [ Js.import_
            (Js.specifiers
              $ concat
                [ [ Js.importSpecifier2 (Js.identifier "extendCollection") (Js.identifier "originalExtendCollection")
                  , Js.importSpecifier1 (Js.identifier "defineCollection")
                  ]
                , map (Js.importSpecifier1 <<< Js.identifier) functionNames
                ])
            (Js.identifier "aeria")
        , Js.exportNamed
          (Js.variable
            (Js.identifier collectionName)
            (Js.call
              (Js.identifier "defineCollection")
              [collection']))
        , Js.exportNamed
            (Js.variable
              (Js.identifier "extendCollection")
              (Js.arrowFunction
                [Js.identifier "collection"]
                (Js.call (Js.identifier "originalExtendCollection")
                  [Js.identifier collectionName, Js.identifier "collection"]
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
  , security
  , presets
  , temporary
  , functions
  , immutable
  }
) = go
  where
  go = Js.object $ concat
    [ cDescription
    , cFunctions'
    , cSecurity'
    ]

  cFunctions' = cConditional cFunctions "functions" functions
  cSecurity' = cConditional cSecurity "security" security

  cDescription =
    [ Js.objectProperty2 "description" $ Js.object $ concat
      [ baseDescription
      , iconDescription
      , ownedDescription
      , timestampsDescription
      , tableDescription
      , writableDescription
      , tableMetaDescription
      , requiredDescription
      , filtersDescription
      , formDescription
      , presetsDescription
      , indexesDescription
      , temporaryDescription
      , searchDescription
      , immutableDescription
      , filtersPresetsDescription
      , layoutDescription
      ]
    ]

  baseDescription =
    [ Js.objectProperty2 "$id" (cCollectionName name)
    , Js.objectProperty2 "properties" (cCollectionProperties properties getters)
    ]

  iconDescription = cIcon icon
  ownedDescription = cOwned owned
  timestampsDescription = cTimestamps timestamps
  searchDescription = cSearch search
  immutableDescription = cImmutable immutable
  temporaryDescription = cTemporary temporary
  formDescription = cConditional cForm "form" form
  tableDescription = cConditional cTable "table" table
  layoutDescription = cConditional cLayout "layout" layout
  filtersDescription = cConditional cFilters "filters" filters
  indexesDescription = cConditional cIndexes "indexes" indexes
  presetsDescription = cConditional cPresets "presets" presets
  writableDescription = cConditional cWritable "writable" writable
  requiredDescription = cConditional cRequired "required" required
  tableMetaDescription = cConditional cTableMeta "tableMeta" tableMeta
  filtersPresetsDescription = cConditional cFiltersPresets "filtersPresets" filtersPresets

cConditional :: forall a. (L.List a -> Js.JsTree) -> String -> L.List a -> Array Js.JsObjectProperty
cConditional f key value = case value of
  L.Nil -> []
  _ -> [Js.objectProperty2 key (f value)]

cImmutable :: Maybe CollectionImmutable -> Array Js.JsObjectProperty
cImmutable Nothing = []
cImmutable (Just (CollectionImmutableBool bool)) = [Js.objectProperty2 "immutable" (Js.boolean bool)]
cImmutable (Just (CollectionImmutableList immutable)) =
  cConditional cImmutable' "immutable" immutable
  where
    cImmutable' immutable' = cPropertiesList (map (\(ImmutableItem _ propertyName) -> propertyName) immutable')

cLayout :: CollectionLayout -> Js.JsTree
cLayout layout = Js.object $ L.toUnfoldable $ map go layout
  where
    go (LayoutItem { name: (PropertyName _ name), component, span_, if_, verticalSpacing  }) =
      Js.objectProperty2 name (
        Js.object $ concat
          [ cMaybe (\label' -> [Js.objectProperty2 "span" (Js.float label')]) span_ []
          , cMaybe (\label' -> [Js.objectProperty2 "verticalSpacing" (Js.float label')]) verticalSpacing []
          , cMaybe (\(Cond _ if_') -> [Js.objectProperty2 "if" (cExpr if_')]) if_ []
          , cMaybe (\component' -> [Js.objectProperty2 "component" (cComponent component')]) component []
          ]
      )

    cComponent (LayoutItemComponent {name, props}) = Js.object
      $ concat
        [ cMaybe (\name' -> [Js.objectProperty2 "name" (Js.string name')]) name []
        , cMaybe (\(Macro _ props') -> [Js.objectProperty2 "props" (Js.code props')]) props []
        ]

cIcon :: Maybe CollectionIcon -> Array Js.JsObjectProperty
cIcon Nothing = []
cIcon (Just (CollectionIcon icon)) = [Js.objectProperty2 "icon" (Js.string icon)]

cOwned :: Maybe CollectionOwned -> Array Js.JsObjectProperty
cOwned Nothing = []
cOwned (Just (CollectionOwned bool)) = [Js.objectProperty2 "owned" (Js.boolean bool)]

cTimestamps :: Maybe CollectionTimestamps -> Array Js.JsObjectProperty
cTimestamps Nothing = []
cTimestamps (Just (CollectionTimestamps timestamps)) = [Js.objectProperty2 "timestamps" (Js.boolean timestamps)]

cSearch :: Maybe CollectionSearch -> Array Js.JsObjectProperty
cSearch Nothing = []
cSearch (Just (CollectionSearch { placeholder, indexes }))
  = [ Js.objectProperty2 "search"
        $ Js.object $
          concat
            [ [ Js.objectProperty2 "indexes" (cPropertiesList indexes)
              ]
            , cMaybe (\placeholder' -> [Js.objectProperty2 "placeholder" (Js.string placeholder')]) placeholder []
            ]
    ]

cPropertiesList :: L.List PropertyName -> Js.JsTree
cPropertiesList list = Js.array (L.toUnfoldable $ map cPropertyName list)

cFiltersPresets :: CollectionFiltersPresets -> Js.JsTree
cFiltersPresets filtersPresets = Js.object $ L.toUnfoldable $ map go filtersPresets
  where
    go (FiltersPresetsItem { name: (PropertyName _ name), label, badgeFunction, filters  }) =
      Js.objectProperty2 name (
        Js.object $ concat
          [ cMaybe (\label' -> [Js.objectProperty2 "name" (Js.string label')]) label []
          , cMaybe (\badgeFunction' -> [Js.objectProperty2 "badgeFunction" (Js.string badgeFunction')]) badgeFunction []
          , cMaybe (\(Macro _ code') -> [Js.objectProperty2 "filters" (Js.code code')]) filters []
          ]
      )

cWritable :: CollectionWritable -> Js.JsTree
cWritable writable = cPropertiesList (map (\(WritableItem _ propertyName) -> propertyName) writable)

functionName ∷ FunctionItem → String
functionName (FunctionItem _ (PropertyName _ function) _) = function

cFunctions :: CollectionFunctions -> Js.JsTree
cFunctions functions = Js.object
  $ L.toUnfoldable
  $ functions
    # L.filter (\(FunctionItem _ _ custom) -> not custom)
    # map (\function -> Js.objectProperty1 (functionName function))

cSecurity :: CollectionSecurity -> Js.JsTree
cSecurity secutiry = Js.object $ L.toUnfoldable $ map go secutiry
  where
    go (SecurityItem
      { functionName: (PropertyName _ name)
      , rateLimiting
      , logging
      }) =
      Js.objectProperty2 name (
        Js.object $ concat
          [ cSecurityRateLimiting rateLimiting
          , cSecurityLogging logging
          ]
      )

    cSecurityRateLimiting Nothing = []
    cSecurityRateLimiting (Just (SecurityRateLimiting {strategy, scale})) =
        [Js.objectProperty2 "rateLimiting"
            $ Js.object $ concat
              [ cMaybe (\strategy' -> [Js.objectProperty2 "strategy" (Js.string strategy')]) strategy []
              , cMaybe (\scale' -> [Js.objectProperty2 "scale" (Js.int scale')]) scale []
              ]]

    cSecurityLogging Nothing = []
    cSecurityLogging (Just (SecurityLogging { strategy })) =
      [Js.objectProperty2 "logging"
        $ Js.object $ concat
          [ cMaybe (\strategy' -> [Js.objectProperty2 "strategy" (Js.string strategy')]) strategy []
          ]]

cTemporary :: Maybe CollectionTemporary -> Array Js.JsObjectProperty
cTemporary Nothing = []
cTemporary (Just (CollectionTemporary {index: (PropertyName _ index), expireAfterSeconds})) =
  [ Js.objectProperty2 "temporary"
    (Js.object
      [ Js.objectProperty2 "index" (Js.string index)
      , Js.objectProperty2 "expireAfterSeconds" (Js.int expireAfterSeconds)
      ]
    )
  ]

cTable :: CollectionTable -> Js.JsTree
cTable table = cPropertiesList (map (\(TableItem _ propertyName) -> propertyName) table)

cPresets :: CollectionPresets -> Js.JsTree
cPresets presets = cPropertiesList (map (\(PresetItem _ propertyName) -> propertyName) presets)

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
              Js.objectProperty2 propertyName (cObject expr)
          )
          required
  where
  hasCondition = L.all (\(Required _ _ cond) -> isNothing cond) required

  cObject Nothing = Js.boolean true

  cObject (Just (Cond _ cond)) = cExpr cond

cBinaryExpr :: String -> Js.JsTree -> Js.JsTree -> Js.JsTree
cBinaryExpr oper e1 e2 =
  Js.object
    [ Js.objectProperty2 "operator" (Js.string oper)
    , Js.objectProperty2 "term1" e1
    , Js.objectProperty2 "term2" e2
    ]

cUnaryExpr :: String -> Js.JsTree -> Js.JsTree
cUnaryExpr oper e1 =
  Js.object
    [ Js.objectProperty2 "operator" (Js.string oper)
    , Js.objectProperty2 "term1" e1
    ]

cExpr :: Expr -> Js.JsTree
cExpr (ELiteral value) = cLiteral value
cExpr (EExists e1) = cUnaryExpr "exists" (cExpr e1)
cExpr (ENot e1) = cUnaryExpr "not" (cExpr e1)
cExpr (EOr e1 e2) =
  Js.object
    [ Js.objectProperty2 "or" (Js.array [cExpr e1, cExpr e2])
    ]
cExpr (EAnd e1 e2) =
  Js.object
    [ Js.objectProperty2 "and" (Js.array [cExpr e1, cExpr e2])
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
          Js.objectProperty2 propertyName (cGetter macro)

        cGetter :: Macro -> Js.JsTree
        cGetter (Macro _ code) = Js.object
          [ Js.objectProperty2 "getter" (Js.arrowFunction [Js.identifier "doc"] (Js.code code))
          ]

    cProperties :: CollectionProperties -> Array Js.JsObjectProperty
    cProperties properties' = L.toUnfoldable $ map go properties'
      where
        go :: Property -> Js.JsObjectProperty
        go property@(Property { name: (PropertyName _ propertyName)})
          = Js.objectProperty2 propertyName (cProperty property)

        cProperty :: Property -> Js.JsTree
        cProperty property@(Property { type_, attributes }) =
          Js.object $ concat [props, attrs]
          where
            props = cPropertyType property type_
            attrs = cAttributes $ L.filter (\(Attribute _ (AttributeName _ attributeName) _) -> attributeName /= "options") attributes

        cPropertyType :: Property -> PropertyType -> Array Js.JsObjectProperty
        cPropertyType property type_ =
          case type_ of
            PFloat _ ->  [cType "number"]
            PInteger _ ->  [cType "integer"]
            PString _ ->  [cType "string"]
            PBoolean _ ->  [cType "boolean"]
            PEnum _ ->
              [ Js.objectProperty2 "enum" (cEnumType property)
              ]
            PArray _ type_' ->
              [ cType "array"
              , cArrayType property type_'
              ]
            PObject _ required properties'' ->
              [ cType "object"
              , Js.objectProperty2 "required" (cRequired required)
              , Js.objectProperty2 "properties" (Js.object $ cProperties properties'')
              ]
            PRef _ collectionName ->
              [ Js.objectProperty2 "$ref" (cCollectionName collectionName)
              ]

        cType type_' = Js.objectProperty2 "type" (Js.string type_')

        cAttributes :: Attributes -> Array Js.JsObjectProperty
        cAttributes attributes = L.toUnfoldable $ map go' attributes
          where
            go' (Attribute _ (AttributeName _ attributeName) attributeValue) =
              case attributeValue of
                ALiteral _ literal -> Js.objectProperty2 attributeName (cLiteral literal)
                AExpr _ expr -> Js.objectProperty2 attributeName (cExpr expr)

        cEnumType (Property { attributes }) = Js.array cOptions
          where
          cOptions =
            case L.find (\(Attribute _ (AttributeName _ x) _) -> x == "options") attributes of
              Just (Attribute _ _ (ALiteral _ (LArray _ value))) ->
                L.toUnfoldable $ map cLiteral value
              _ -> []

        cArrayType property type_' =
          Js.objectProperty2 "items" $ Js.object $ cPropertyType property type_'

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
