module Aeria.Codegen where

import Prelude

import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Type (typegen)
import Aeria.Codegen.Typescript.Tree as Ts
import Aeria.Syntax.Tree (ActionItem(..), AdditionalProperties(..), Attribute(..), AttributeName(..), AttributeValue(..), Attributes, Collection(..), CollectionActions, CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionFormLayout, CollectionFunctions, CollectionGetters, CollectionIcon(..), CollectionImmutable(..), CollectionIndexes, CollectionLayout(..), CollectionName(..), CollectionOwned(..), CollectionPreferred, CollectionPresets, CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionSecurity, CollectionTable, CollectionTableLayout, CollectionTableMeta, CollectionTemporary(..), CollectionTimestamps(..), CollectionWritable, Cond(..), Expr(..), ExtendsName(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), FunctionItem(..), FunctionName(..), Getter(..), ImmutableItem(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), LayoutOptions(..), Literal(..), Macro(..), PreferredItem(..), PresetItem(..), Program(..), Property(..), PropertyName(..), PropertyType(..), RequireItem(..), Required(..), SecurityItem(..), SecurityLogging(..), SecurityRateLimiting(..), TableItem(..), TableLayoutItem(..), TableMetaItem(..), WritableItem(..))
import Control.Lazy (fix)
import Data.Array (concat, elem, head, union)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String.Utils (ucLower)

data Codegen
  = Codegen String Js.JsStatements Ts.TsStatements

codegen :: Program -> L.List Codegen
codegen (Program { collections }) = map go collections
  where
  go collection@(Collection { name: c@(CollectionName _ collectionName), functions, extends }) =
    Codegen collectionName' jsFile tsFile
    where
    collectionName' = getCollectionName c
    functions' = L.toUnfoldable
      $ functions
        # L.filter (\(FunctionItem { custom }) -> not custom)
        # map (\(FunctionItem { functionName }) -> getFunctionName functionName)

    tsImportsFunctions = map (Ts.importSpecifier <<< Ts.identifier) functions'
    jsImportsFunctions = map (Js.importSpecifier1 <<< Js.identifier) functions'

    collection' = cCollection collection
    collectionType = typegen collection'

    tsFile =
      Ts.statements
        [ Ts.import_
            (Ts.specifiers
              $ concat
                [ [ Ts.importSpecifier (Ts.identifier "Collection")
                  , Ts.importSpecifier (Ts.identifier "SchemaWithId")
                  , Ts.importSpecifier (Ts.identifier "ExtendCollection")
                  ]
                , tsImportsFunctions
                ]
              )
            (Ts.identifier "aeria")

        , (case extends of
          Just (ExtendsName package collection'') ->
            Ts.import_
              (Ts.specifiers
                [ Ts.importSpecifier2 (Ts.identifier $ ucLower collection'') (Ts.identifier "original")
                ])
              (Ts.identifier package)
          Nothing -> Ts.emptyStatement)

        , Ts.exportNamed
            (Ts.typeAlias
              (Ts.identifier (collectionName' <> "Collection"))
              (if isJust extends then
                (Ts.typeReference
                        [ Ts.typeParameter $ Ts.typeQuery (Ts.identifier "original")
                        , Ts.typeParameter collectionType
                        ]
                        (Ts.identifier "ExtendCollection"))
                else collectionType))

        , Ts.exportNamed
            (Ts.variable
              [Ts.declareKeyword, Ts.constKeyword]
              (Ts.identifier collectionName')
              (Ts.intersectionType
                (Ts.typeReference [] (Ts.identifier (collectionName' <> "Collection")))
                (Ts.typeLiteral
                  (Ts.typeLitObject
                    [Ts.typeObjectProperty
                      (Ts.identifier "item")
                      (Ts.typeReference
                        [ Ts.typeParameter (Ts.typeReference [] (Ts.identifier (collectionName' <> "Collection[\"description\"]")))
                        ]
                        (Ts.identifier "SchemaWithId"))]))))

        , Ts.exportNamed
          (Ts.typeAlias
            (Ts.identifier collectionName)
            ( Ts.typeReference
              [Ts.typeParameter $ Ts.typeQuery (Ts.identifier (collectionName' <> ".description"))]
              (Ts.identifier "SchemaWithId")
            )
          )

        , Ts.exportNamed
            (Ts.variable
              [Ts.declareKeyword, Ts.constKeyword]
              (Ts.identifier $ "extend" <> collectionName <> "Collection")
              (Ts.functionType
                [ Ts.typeParameter
                    (Ts.typeExtends
                      (Ts.typeReference [] (Ts.identifier "const TCollection"))
                      (Ts.typeReference [] (Ts.identifier "{ [P in keyof Collection]?: Partial<Collection[P]> }")))]
                [Ts.parameter
                  (Ts.identifier "collection")
                  (Ts.typeReference [] (Ts.identifier "TCollection")) ]
                  (Ts.typeReference
                    [ Ts.typeParameter $ Ts.typeQuery (Ts.identifier collectionName')
                    , Ts.typeParameter $ Ts.typeReference [] (Ts.identifier "TCollection")
                    ]
                    (Ts.identifier "ExtendCollection"))))]

    jsFile =
      Js.statements
        [ Js.import_
            (Js.specifiers
              $ concat
                [ [ Js.importSpecifier1 (Js.identifier "extendCollection")
                  , Js.importSpecifier1 (Js.identifier "defineCollection")
                  ]
                , jsImportsFunctions
                ])
            (Js.identifier "aeria")

        , (case extends of
          Just (ExtendsName package collection'') ->
            Js.import_
              (Js.specifiers
                [ Js.importSpecifier2 (Js.identifier $ ucLower collection'') (Js.identifier "original")
                ])
              (Js.identifier package)
          Nothing -> Js.emptyStatement)

        , Js.exportNamed
          (Js.variable
            (Js.identifier collectionName')

            if isJust extends then
              (Js.call
                  (Js.identifier "extendCollection")
                  [Js.identifier "original", collection'])
              else
                (Js.call
                  (Js.identifier "defineCollection")
                  [collection']))

        , Js.exportNamed
            (Js.variable
              (Js.identifier $ "extend" <> collectionName <> "Collection")
              (Js.arrowFunction
                [Js.identifier "collection"]
                (Js.call (Js.identifier "extendCollection")
                  [Js.identifier collectionName', Js.identifier "collection"]
                )))]

cCollection :: Collection -> Js.JsTree
cCollection (Collection
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
  , formLayout
  , filtersPresets
  , layout
  , tableLayout
  , writable
  , preferred
  , security
  , actions
  , individualActions
  , presets
  , temporary
  , functions
  , immutable
  }
) = collectionProperties
    [ collectionDescription
    , collectionFunctions
    , collectionExposedFunctions
    , collectionSecurity
    ]
  where
  collectionFunctions = collectionPropertyL "functions" cFunctions functions
  collectionExposedFunctions = collectionPropertyL "exposedFunctions" cExposedFunctions
    (L.filter (\(FunctionItem { expose }) -> isJust expose) functions)
  collectionSecurity = collectionPropertyL "security" cSecurity security
  collectionDescription =
    [ Js.objectProperty2 "description" $
      collectionProperties
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
        , formLayoutDescription
        , indexesDescription
        , tableLayoutDescription
        , preferredDescription
        , temporaryDescription
        , searchDescription
        , immutableDescription
        , individualActionsDescription
        , filtersPresetsDescription
        , actionsDescription
        , layoutDescription
        ]
    ]

  baseDescription =
    [ Js.objectProperty2 "$id" (cCollectionName name)
    , Js.objectProperty2 "properties" (cCollectionProperties properties getters)
    ]

  iconDescription               = collectionPropertyM "icon" cIcon icon
  ownedDescription              = collectionPropertyM "owned" cOwned owned
  timestampsDescription         = collectionPropertyM "timestamps" cTimestamps timestamps
  searchDescription             = collectionPropertyM "search" cSearch search
  immutableDescription          = collectionPropertyM "immutable" cImmutable immutable
  temporaryDescription          = collectionPropertyM "temporary" cTemporary temporary
  formDescription               = collectionPropertyL "form" cForm form
  tableDescription              = collectionPropertyL "table" cTable table
  layoutDescription             = collectionPropertyM "layout" cLayout layout
  tableLayoutDescription        = collectionPropertyL "tableLayout" cTableLayout tableLayout
  formLayoutDescription         = collectionPropertyL "formLayout" cFormLayout formLayout
  actionsDescription            = collectionPropertyL "actions" cActions actions
  individualActionsDescription  = collectionPropertyL "individualActions" cActions individualActions
  filtersDescription            = collectionPropertyL "filters" cFilters filters
  indexesDescription            = collectionPropertyL "indexes" cIndexes indexes
  presetsDescription            = collectionPropertyL "presets" cPresets presets
  writableDescription           = collectionPropertyL "writable" cWritable writable
  requiredDescription           = collectionPropertyL "required" cRequired required
  tableMetaDescription          = collectionPropertyL "tableMeta" cTableMeta tableMeta
  filtersPresetsDescription     = collectionPropertyL "filtersPresets" cFiltersPresets filtersPresets
  preferredDescription          = collectionPropertyL "preferred" cPreferred preferred

collectionPropertyM :: forall a. String -> (a -> Js.JsTree) -> Maybe a -> Array Js.JsObjectProperty
collectionPropertyM k f x =
  case x of
    Just a -> [Js.objectProperty2 k (f a)]
    Nothing -> []

collectionPropertyL :: forall a. String -> (L.List a -> Js.JsTree) -> L.List a -> Array Js.JsObjectProperty
collectionPropertyL k f x =
  case x of
    L.Nil -> []
    _ -> [Js.objectProperty2 k (f x)]

cPropertyNameL :: L.List PropertyName -> Js.JsTree
cPropertyNameL list =
  map cPropertyName list
    # L.toUnfoldable
    # Js.array

collectionProperties :: Array (Array Js.JsObjectProperty) -> Js.JsTree
collectionProperties = Js.object <<< concat

cImmutable :: CollectionImmutable -> Js.JsTree
cImmutable (CollectionImmutableBool bool) = Js.boolean bool
cImmutable (CollectionImmutableList immutable) =
  cPropertyNameL (map (\(ImmutableItem _ propertyName) -> propertyName) immutable)

cIcon :: CollectionIcon -> Js.JsTree
cIcon (CollectionIcon icon) = Js.string icon

cOwned :: CollectionOwned -> Js.JsTree
cOwned (CollectionOwned owned) = Js.boolean owned

cTimestamps :: CollectionTimestamps -> Js.JsTree
cTimestamps (CollectionTimestamps timestamps) = Js.boolean timestamps

cSearch :: CollectionSearch -> Js.JsTree
cSearch (CollectionSearch { placeholder, indexes }) =
  collectionProperties
    [ [ Js.objectProperty2 "indexes" (cPropertyNameL indexes)
      ]
    , collectionPropertyM "placeholder" Js.string placeholder
    ]

cTemporary :: CollectionTemporary -> Js.JsTree
cTemporary (CollectionTemporary {index: (PropertyName _ index), expireAfterSeconds}) =
  Js.object
    [ Js.objectProperty2 "index" (Js.string index)
    , Js.objectProperty2 "expireAfterSeconds" (Js.int expireAfterSeconds)
    ]

cTable :: CollectionTable -> Js.JsTree
cTable table = cPropertyNameL (map (\(TableItem _ propertyName) -> propertyName) table)

cPresets :: CollectionPresets -> Js.JsTree
cPresets presets = cPropertyNameL (map (\(PresetItem _ propertyName) -> propertyName) presets)

cTableMeta :: CollectionTableMeta -> Js.JsTree
cTableMeta tableMeta = cPropertyNameL (map (\(TableMetaItem _ propertyName) -> propertyName) tableMeta)

cForm :: CollectionForm -> Js.JsTree
cForm form = cPropertyNameL (map (\(FormItem _ propertyName) -> propertyName) form)

cFilters :: CollectionFilters -> Js.JsTree
cFilters filters = cPropertyNameL (map (\(FilterItem _ propertyName) -> propertyName) filters)

cIndexes :: CollectionIndexes -> Js.JsTree
cIndexes indexes = cPropertyNameL (map (\(IndexesItem _ propertyName) -> propertyName) indexes)

cWritable :: CollectionWritable -> Js.JsTree
cWritable writable = cPropertyNameL (map (\(WritableItem _ propertyName) -> propertyName) writable)

cPreferred :: CollectionPreferred -> Js.JsTree
cPreferred preferred =
  map go preferred
    # L.toUnfoldable
    # Js.object
  where
  go (PreferredItem
    { role
    , tableMeta
    , actions
    , individualActions
    , filters
    , filtersPresets
    , layout
    , table
    , form
    , tableLayout
    , formLayout
    }) =
      Js.objectProperty2 role
        ( collectionProperties
          [ collectionPropertyL "tableMeta" cTableMeta tableMeta
          , collectionPropertyL "form" cForm form
          , collectionPropertyL "table" cTable table
          , collectionPropertyM "layout" cLayout layout
          , collectionPropertyL "tableLayout" cTableLayout tableLayout
          , collectionPropertyL "formLayout" cFormLayout formLayout
          , collectionPropertyL "actions" cActions actions
          , collectionPropertyL "individualActions" cActions individualActions
          , collectionPropertyL "filters" cFilters filters
          , collectionPropertyL "filtersPresets" cFiltersPresets filtersPresets
          ]
        )

-- cFormLayout :: CollectionFormLayout -> Js.JsTree
-- cFormLayout collectionFormLayout = Js.object
--   [ (cLayout collectionLayout)]
cFormLayout :: CollectionFormLayout -> Js.JsTree
cFormLayout collectionFormLayout = Js.object
  [Js.objectProperty2 "fields" $
    map go collectionFormLayout
      # L.toUnfoldable
      # Js.object]
  where
    go (LayoutItem { name, component, span_, if_, verticalSpacing  }) =
      Js.objectProperty2 (getPropertyName name) $
        collectionProperties
          [ collectionPropertyM "span" Js.float span_
          , collectionPropertyM "verticalSpacing" Js.float verticalSpacing
          , collectionPropertyM "if" (\(Cond _ expr) -> cExpr expr) if_
          , collectionPropertyM "component" cComponent component
          ]

    cComponent (LayoutItemComponent { name, props }) =
      collectionProperties
        [ collectionPropertyM "name" Js.string name
        , collectionPropertyM "props" (\(Macro _ code) -> Js.code code) props
        ]

cActions :: CollectionActions -> Js.JsTree
cActions actions =
  map go actions
    # L.toUnfoldable
    # Js.object
  where
    go (ActionItem
    { actionName
    , label
    , icon
    , ask
    , selection
    , effect
    , button
    , translate
    , setItem
    , fetchItem
    , clearItem
    , params
    , query
    , requires
    }) =
      Js.objectProperty2 (getPropertyName actionName) $
        collectionProperties
          [ collectionPropertyM "label" Js.string label
          , collectionPropertyM "icon" Js.string icon
          , collectionPropertyM "ask" Js.boolean ask
          , collectionPropertyM "selection" Js.boolean selection
          , collectionPropertyM "effect" Js.string effect
          , collectionPropertyM "translate" Js.boolean translate
          , collectionPropertyM "fetchItem" Js.boolean fetchItem
          , collectionPropertyM "button" Js.boolean button
          , collectionPropertyM "setItem" Js.boolean setItem
          , collectionPropertyM "clearItem" Js.boolean clearItem
          , collectionPropertyM "params" (\(Macro _ code) -> Js.code code) params
          , collectionPropertyM "query" (\(Macro _ code) -> Js.code code) query
          , collectionPropertyL "requires" cPropertyNameL (map (\(RequireItem _ require) -> require) requires)
          ]

cLayout :: CollectionLayout -> Js.JsTree
cLayout (CollectionLayout { name, options }) =
  collectionProperties
    [ [Js.objectProperty2 "name" $ Js.string name]
    , case options of
      Just (LayoutOptions {title, badge, picture, information, active, translateBadge}) ->
        [Js.objectProperty2 "options" $ collectionProperties
          [ collectionPropertyM "title" (Js.string <<< getPropertyName) title
          , collectionPropertyM "badge" (Js.string <<< getPropertyName) badge
          , collectionPropertyM "picture" (Js.string <<< getPropertyName) picture
          , collectionPropertyM "information" (Js.string <<< getPropertyName) information
          , collectionPropertyM "active" (Js.string <<< getPropertyName) active
          , collectionPropertyM "translateBadge" Js.boolean translateBadge
          ]]
      Nothing -> []
    ]

cTableLayout :: CollectionTableLayout -> Js.JsTree
cTableLayout tableLayout =
  Js.object
    [ Js.objectProperty2 "actions" $
        map go tableLayout
          # L.toUnfoldable
          # Js.object
    ]
  where
    go (TableLayoutItem { actionName, route, if_, button, action }) =
      let
        baseObjectProperties =
          [ collectionPropertyM "if" (\(Cond _ expr) -> cExpr expr) if_
          , collectionPropertyM "button" cButton button
          ]

        properties = collectionProperties $
          case cActions (L.singleton action) of
            Js.JSLiteral (Js.JSObject action') ->
              case head action' of
                Just (Js.JsObjectProperty2 _ (Js.JSLiteral (Js.JSObject actionProperties))) ->
                  union baseObjectProperties [actionProperties]
                _ -> baseObjectProperties
            _ -> []

      in case route of
          Just route' -> Js.objectProperty2' (Js.string route') properties
          Nothing -> Js.objectProperty2 (getPropertyName actionName) properties

    cButton (Left bool) = Js.boolean bool
    cButton (Right (Cond _ expr)) = cExpr expr

cFiltersPresets :: CollectionFiltersPresets -> Js.JsTree
cFiltersPresets filtersPresets =
  map go filtersPresets
    # L.toUnfoldable
    # Js.object
  where
    go (FiltersPresetsItem { name, label, badgeFunction, filters  }) =
      Js.objectProperty2 (getPropertyName name) $
        collectionProperties
          [ collectionPropertyM "name" Js.string label
          , collectionPropertyM "badgeFunction" Js.string badgeFunction
          , collectionPropertyM "filters" (\(Macro _ code) -> Js.code code) filters
          ]

cFunctions :: CollectionFunctions -> Js.JsTree
cFunctions functions = Js.object
  $ L.toUnfoldable
  $ functions
    # L.filter (\(FunctionItem { custom }) -> not custom)
    # map (\(FunctionItem { functionName }) -> Js.objectProperty1 (getFunctionName functionName))

cExposedFunctions :: CollectionFunctions -> Js.JsTree
cExposedFunctions functions = Js.object
  $ L.toUnfoldable
  $ functions
    # map (\(FunctionItem { functionName, expose }) ->
      case expose of
        Just (Attribute _ _ attributeValue) ->
          case attributeValue of
            ALiteral _ literal -> Js.objectProperty2 (getFunctionName functionName) (cLiteral literal)
            _ -> Js.objectProperty1 (getFunctionName functionName)
        Nothing -> Js.objectProperty1 (getFunctionName functionName)
    )

cSecurity :: CollectionSecurity -> Js.JsTree
cSecurity secutiry =
  map go secutiry
    # L.toUnfoldable
    # Js.object
  where
    go (SecurityItem
      { functionName
      , rateLimiting
      , logging
      }) =
      Js.objectProperty2 (getFunctionName functionName) (
        collectionProperties
          [ collectionPropertyM "rateLimiting" cSecurityRateLimiting rateLimiting
          , collectionPropertyM "logging" cSecurityLogging logging
          ]
      )

    cSecurityRateLimiting (SecurityRateLimiting { strategy, scale }) =
      collectionProperties
        [ collectionPropertyM "strategy" Js.string strategy
        , collectionPropertyM "scale" Js.int scale
        ]

    cSecurityLogging (SecurityLogging { strategy }) =
      collectionProperties
        [ collectionPropertyM "strategy" Js.string strategy
        ]

cRequired :: CollectionRequired -> Js.JsTree
cRequired required =
  if hasCondition then
    map (\(Required _ propertyName _) -> cPropertyName propertyName) required
      # L.toUnfoldable
      # Js.array
  else
    map (\(Required _ propertyName expr) -> Js.objectProperty2 (getPropertyName propertyName) (cObject expr)) required
      # L.toUnfoldable
      # Js.object
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
cExpr (ETruthy e1) = cUnaryExpr "truthy" (cExpr e1)
-- cExpr (EExists e1) = cUnaryExpr "exists" (cExpr e1)
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
        go (Getter { name, macro }) =
          Js.objectProperty2 (getPropertyName name) (cGetter macro)

        cGetter :: Macro -> Js.JsTree
        cGetter (Macro _ code) = Js.object
          [ Js.objectProperty2 "getter" (Js.arrowFunction [Js.identifier "doc"] (Js.code code))
          ]

    cProperties :: CollectionProperties -> Array Js.JsObjectProperty
    cProperties properties' = L.toUnfoldable $ map go properties'
      where
        go :: Property -> Js.JsObjectProperty
        go property@(Property { name })
          = Js.objectProperty2 (getPropertyName name) (cProperty property)

        cProperty :: Property -> Js.JsTree
        cProperty (Property { type_, attributes }) = collectionProperties [type_', attributes']
          where
            type_' = cPropertyType type_
            attributes' = cAttributes type_ attributes

        cPropertyType :: PropertyType -> Array Js.JsObjectProperty
        cPropertyType propertyType =
          case propertyType of
            PConst _ -> []
            PEnum _ -> []
            PNum _ ->  [cType "number"]
            PInteger _ ->  [cType "integer"]
            PString _ ->  [cType "string"]
            PBoolean _ ->  [cType "boolean"]
            PArray _ _ -> [cType "array"]
            PObject _ required properties'' additionalProperties ->
              concat
                [ collectionPropertyL "required" cRequired required
                , collectionPropertyM "additionalProperties" (\(AdditionalProperties p)  -> Js.object $ cPropertyType p) additionalProperties
                , [ cType "object"
                  , Js.objectProperty2 "properties" (Js.object $ cProperties properties'')
                  ]
                ]
            PRef _ collectionName ->
              [ Js.objectProperty2 "$ref" (cCollectionName collectionName)
              ]

        cType type_' = Js.objectProperty2 "type" (Js.string type_')

        cAttributes :: PropertyType -> Attributes -> Array Js.JsObjectProperty
        cAttributes propertyType attributes =
          case propertyType of
            PConst _ ->
              let const = L.find (\(Attribute _ attributeName _) -> getAttributeName attributeName == "value") attributes
              in case const of
                Just (Attribute _ _ (ALiteral _ literal)) ->
                  [ Js.objectProperty2 "const" (cLiteral literal)
                  ]
                _ -> []
            PEnum _ ->
              let values = L.find (\(Attribute _ attributeName _) -> getAttributeName attributeName == "values") attributes
              in case values of
                Just (Attribute _ _ (ALiteral _ (LArray _ value))) ->
                  [ Js.objectProperty2 "enum" (Js.array $ L.toUnfoldable $ map cLiteral value)
                  ]
                _ -> []
            PArray _ type_' ->
              let
                typeAttributes = L.filter (\(Attribute _ attributeName _) -> not (getAttributeName attributeName `elem` arrayAttributes)) attributes
                arrayAttributes' = L.filter (\(Attribute _ attributeName _) -> getAttributeName attributeName `elem` arrayAttributes) attributes
                typeAttributes' =
                  [concat [cPropertyType type_', cAttributes type_' typeAttributes]
                    # Js.object
                    # Js.objectProperty2 "items"]
                arrayAttributes'' = L.toUnfoldable $ map go' arrayAttributes'
              in concat [typeAttributes', arrayAttributes'']
            _ -> L.toUnfoldable $ map go' attributes
          where
            go' (Attribute _ attributeName attributeValue) =
              let attributeName' = getAttributeName attributeName
              in case attributeValue of
                ALiteral _ literal -> Js.objectProperty2 attributeName' (cLiteral literal)
                AExpr _ expr -> Js.objectProperty2 attributeName' (cExpr expr)

        arrayAttributes =
          [ "default"
          , "minItems"
          , "maxItems"
          , "uniqueItems"
          ]

cLiteral :: Literal -> Js.JsTree
cLiteral =
  fix \self -> case _ of
    LUndefined _ -> Js.undefined
    LNull _ -> Js.null
    LInteger _ i -> Js.int i
    LNum _ f -> Js.float f
    LString _ s -> Js.string s
    LBoolean _ b -> Js.boolean b
    LArray _ a -> Js.array (L.toUnfoldable $ map self a)
    LProperty _ propertyName -> cPropertyName propertyName

cCollectionName :: CollectionName -> Js.JsTree
cCollectionName name =
  name
    # getCollectionName
    # Js.string

cPropertyName :: PropertyName -> Js.JsTree
cPropertyName name =
  name
    # getPropertyName
    # Js.string

getFunctionName ∷ FunctionName -> String
getFunctionName (FunctionName _ functionName) = functionName

getCollectionName :: CollectionName -> String
getCollectionName (CollectionName _ collectionName) = ucLower collectionName

getPropertyName :: PropertyName -> String
getPropertyName (PropertyName _ propertyName) = propertyName

getAttributeName :: AttributeName -> String
getAttributeName (AttributeName _ attributeName) = attributeName
