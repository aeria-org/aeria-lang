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
import Data.Tuple.Nested ((/\))

data Codegen
  = Codegen String Js.Statements Ts.TsStatements

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

    tsImportsFunctions = map (Ts.importSpecifier1 <<< Ts.identifier) functions'
    jsImportsFunctions = map (Js.importSpecifier1 <<< Js.identifier) functions'

    collection' = cCollection collection
    collectionType = typegen collection'

    tsFile =
      Ts.statements
        [ Ts.importDeclaration
            (Ts.specifiers
              $ concat
                [ [ Ts.importSpecifier1 (Ts.identifier "Collection")
                  , Ts.importSpecifier1 (Ts.identifier "SchemaWithId")
                  , Ts.importSpecifier1 (Ts.identifier "ExtendCollection")
                  ]
                , tsImportsFunctions
                ]
              )
            (Ts.identifier "aeria")

        , (case extends of
          Just (ExtendsName package collection'') ->
            Ts.importDeclaration
              (Ts.specifiers
                [ Ts.importSpecifier2 (Ts.identifier $ ucLower collection'') (Ts.identifier "original")
                ])
              (Ts.identifier package)
          Nothing -> Ts.emptyStatement)

        , Ts.exportDeclaration $ Ts.declareDeclaration
            (Ts.typeAliasDeclaration
              (Ts.identifier (collectionName' <> "Collection"))
              (if isJust extends then
                  Ts.type_
                    (Ts.typeGeneric
                      [ Ts.typeQuery (Ts.variable "original")
                      , collectionType
                      ]
                      (Ts.type_ $ Ts.typeVariable "ExtendCollection"))
                else collectionType))

        , Ts.exportDeclaration $ Ts.declareDeclaration
            (Ts.variableDeclaration
              (Ts.identifier collectionName')
              (Ts.intersection
                (Ts.type_ $ Ts.typeVariable (collectionName' <> "Collection"))
                (Ts.type_
                  (Ts.typeObject
                    [Ts.typeObjectProperty
                      (Ts.identifier "item")
                      (Ts.type_
                        (Ts.typeGeneric
                          [ (Ts.type_ $ Ts.typeRaw (collectionName' <> "Collection[\"description\"]"))
                          ]
                          (Ts.type_ $ Ts.typeVariable "SchemaWithId")))]))))

        , Ts.exportDeclaration $ Ts.declareDeclaration
            (Ts.typeAliasDeclaration
              (Ts.identifier collectionName)
              (Ts.type_ (Ts.typeGeneric
                [Ts.typeQuery (Ts.variable (collectionName' <> ".description"))]
                (Ts.type_ $ Ts.typeVariable  "SchemaWithId")
              ))
            )

        , Ts.exportDeclaration $ Ts.declareDeclaration
            (Ts.variableDeclaration
              (Ts.identifier $ "extend" <> collectionName <> "Collection")
              (Ts.type_ $ Ts.typeGeneric
                [ (Ts.extends
                  (Ts.type_ $ Ts.typeRaw "const TCollection")
                  (Ts.type_ $ Ts.typeRaw "{ [P in keyof Collection]?: Partial<Collection[P]> }"))]
              (Ts.type_ $ Ts.typeFunction
                [ (Ts.identifier "collection") /\ (Ts.type_ $ Ts.typeVariable "TCollection") ]
                (Ts.type_ $ (Ts.typeGeneric
                  [ Ts.typeQuery (Ts.type_ $ Ts.typeVariable collectionName')
                  , Ts.type_ $ Ts.typeVariable "TCollection"
                  ]
                  (Ts.type_ $ Ts.typeVariable "ExtendCollection"))))))]

    jsFile =
      Js.statements
        [ Js.importDeclaration
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
            Js.importDeclaration
              (Js.specifiers
                [ Js.importSpecifier2 (Js.identifier $ ucLower collection'') (Js.identifier "original")
                ])
              (Js.identifier package)
          Nothing -> Js.emptyStatement)

        , Js.exportDeclaration
          (Js.variableDeclaration
            (Js.identifier collectionName')

            if isJust extends then
              (Js.call
                  (Js.variable "extendCollection")
                  [Js.variable "original", collection'])
              else
                (Js.call
                  (Js.variable "defineCollection")
                  [collection']))

        , Js.exportDeclaration
            (Js.variableDeclaration
              (Js.identifier $ "extend" <> collectionName <> "Collection")
              (Js.function
                [Js.identifier "collection"]
                (Js.call (Js.variable "extendCollection")
                  [Js.variable collectionName', Js.variable "collection"]
                )))]

cCollection :: Collection -> Js.Tree
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

collectionPropertyM :: forall a. String -> (a -> Js.Tree) -> Maybe a -> Array Js.ObjectProperty
collectionPropertyM k f x =
  case x of
    Just a -> [Js.objectProperty2 k (f a)]
    Nothing -> []

collectionPropertyL :: forall a. String -> (L.List a -> Js.Tree) -> L.List a -> Array Js.ObjectProperty
collectionPropertyL k f x =
  case x of
    L.Nil -> []
    _ -> [Js.objectProperty2 k (f x)]

cPropertyNameL :: L.List PropertyName -> Js.Tree
cPropertyNameL list =
  map cPropertyName list
    # L.toUnfoldable
    # Js.array

collectionProperties :: Array (Array Js.ObjectProperty) -> Js.Tree
collectionProperties = Js.object <<< concat

cImmutable :: CollectionImmutable -> Js.Tree
cImmutable (CollectionImmutableBool bool) = Js.boolean bool
cImmutable (CollectionImmutableList immutable) =
  cPropertyNameL (map (\(ImmutableItem _ propertyName) -> propertyName) immutable)

cIcon :: CollectionIcon -> Js.Tree
cIcon (CollectionIcon icon) = Js.string icon

cOwned :: CollectionOwned -> Js.Tree
cOwned (CollectionOwned owned) = Js.boolean owned

cTimestamps :: CollectionTimestamps -> Js.Tree
cTimestamps (CollectionTimestamps timestamps) = Js.boolean timestamps

cSearch :: CollectionSearch -> Js.Tree
cSearch (CollectionSearch { placeholder, indexes }) =
  collectionProperties
    [ [ Js.objectProperty2 "indexes" (cPropertyNameL indexes)
      ]
    , collectionPropertyM "placeholder" Js.string placeholder
    ]

cTemporary :: CollectionTemporary -> Js.Tree
cTemporary (CollectionTemporary {index: (PropertyName _ index), expireAfterSeconds}) =
  Js.object
    [ Js.objectProperty2 "index" (Js.string index)
    , Js.objectProperty2 "expireAfterSeconds" (Js.int expireAfterSeconds)
    ]

cTable :: CollectionTable -> Js.Tree
cTable table = cPropertyNameL (map (\(TableItem _ propertyName) -> propertyName) table)

cPresets :: CollectionPresets -> Js.Tree
cPresets presets = cPropertyNameL (map (\(PresetItem _ propertyName) -> propertyName) presets)

cTableMeta :: CollectionTableMeta -> Js.Tree
cTableMeta tableMeta = cPropertyNameL (map (\(TableMetaItem _ propertyName) -> propertyName) tableMeta)

cForm :: CollectionForm -> Js.Tree
cForm form = cPropertyNameL (map (\(FormItem _ propertyName) -> propertyName) form)

cFilters :: CollectionFilters -> Js.Tree
cFilters filters = cPropertyNameL (map (\(FilterItem _ propertyName) -> propertyName) filters)

cIndexes :: CollectionIndexes -> Js.Tree
cIndexes indexes = cPropertyNameL (map (\(IndexesItem _ propertyName) -> propertyName) indexes)

cWritable :: CollectionWritable -> Js.Tree
cWritable writable = cPropertyNameL (map (\(WritableItem _ propertyName) -> propertyName) writable)

cPreferred :: CollectionPreferred -> Js.Tree
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

cFormLayout :: CollectionFormLayout -> Js.Tree
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
        , collectionPropertyM "props" (\(Macro _ code) -> Js.raw code) props
        ]

cActions :: CollectionActions -> Js.Tree
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
          , collectionPropertyM "params" (\(Macro _ code) -> Js.raw code) params
          , collectionPropertyM "query" (\(Macro _ code) -> Js.raw code) query
          , collectionPropertyL "requires" cPropertyNameL (map (\(RequireItem _ require) -> require) requires)
          ]

cLayout :: CollectionLayout -> Js.Tree
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

cTableLayout :: CollectionTableLayout -> Js.Tree
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
            Js.Object action' ->
              case head action' of
                Just (Js.ObjectProperty2 _ (Js.Object actionProperties)) ->
                  union baseObjectProperties [actionProperties]
                _ -> baseObjectProperties
            _ -> []

      in case route of
          Just route' -> Js.objectProperty2' (Js.identifier route') properties
          Nothing -> Js.objectProperty2 (getPropertyName actionName) properties

    cButton (Left bool) = Js.boolean bool
    cButton (Right (Cond _ expr)) = cExpr expr

cFiltersPresets :: CollectionFiltersPresets -> Js.Tree
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
          , collectionPropertyM "filters" (\(Macro _ code) -> Js.raw code) filters
          ]

cFunctions :: CollectionFunctions -> Js.Tree
cFunctions functions = Js.object
  $ L.toUnfoldable
  $ functions
    # L.filter (\(FunctionItem { custom }) -> not custom)
    # map (\(FunctionItem { functionName }) -> Js.objectProperty1 (getFunctionName functionName))

cExposedFunctions :: CollectionFunctions -> Js.Tree
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

cSecurity :: CollectionSecurity -> Js.Tree
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

cRequired :: CollectionRequired -> Js.Tree
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

cBinaryExpr :: String -> Js.Tree -> Js.Tree -> Js.Tree
cBinaryExpr oper e1 e2 =
  Js.object
    [ Js.objectProperty2 "operator" (Js.string oper)
    , Js.objectProperty2 "term1" e1
    , Js.objectProperty2 "term2" e2
    ]

cUnaryExpr :: String -> Js.Tree -> Js.Tree
cUnaryExpr oper e1 =
  Js.object
    [ Js.objectProperty2 "operator" (Js.string oper)
    , Js.objectProperty2 "term1" e1
    ]

cExpr :: Expr -> Js.Tree
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

cCollectionProperties :: CollectionProperties -> CollectionGetters -> Js.Tree
cCollectionProperties properties getters = Js.object $ union (cProperties properties) (cGetters getters)
  where
    cGetters :: CollectionGetters -> Array Js.ObjectProperty
    cGetters getters' = L.toUnfoldable $ map go getters'
      where
        go :: Getter -> Js.ObjectProperty
        go (Getter { name, macro }) =
          Js.objectProperty2 (getPropertyName name) (cGetter macro)

        cGetter :: Macro -> Js.Tree
        cGetter (Macro _ code) = Js.object
          [ Js.objectProperty2 "getter" (Js.function [Js.identifier "doc"] (Js.raw code))
          ]

    cProperties :: CollectionProperties -> Array Js.ObjectProperty
    cProperties properties' = L.toUnfoldable $ map go properties'
      where
        go :: Property -> Js.ObjectProperty
        go property@(Property { name })
          = Js.objectProperty2 (getPropertyName name) (cProperty property)

        cProperty :: Property -> Js.Tree
        cProperty (Property { type_, attributes }) = collectionProperties [type_', attributes']
          where
            type_' = cPropertyType type_
            attributes' = cAttributes type_ attributes

        cPropertyType :: PropertyType -> Array Js.ObjectProperty
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

        cAttributes :: PropertyType -> Attributes -> Array Js.ObjectProperty
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

cLiteral :: Literal -> Js.Tree
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

cCollectionName :: CollectionName -> Js.Tree
cCollectionName name =
  name
    # getCollectionName
    # Js.string

cPropertyName :: PropertyName -> Js.Tree
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
