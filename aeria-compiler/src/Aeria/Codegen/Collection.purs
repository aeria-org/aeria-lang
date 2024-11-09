module Aeria.Codegen.Collection where

import Aeria.Syntax.Tree

import Aeria.Codegen.Javascript.Tree as Js
import Control.Lazy (fix)
import Data.Array (catMaybes, concat, elem, head, union)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), isJust, isNothing)
import Prelude (map, not, (#), ($), (<<<), (<>), (==))

cCollection :: Collection -> Js.Tree
cCollection collection@(Collection { functions, security }) =
  Js.object (union [collectionDescription] collection')
  where
  collection' = catMaybes
    [ collectionFunctions
    , collectionExposedFunctions
    , collectionSecurity
    ]

  exposedFunctions = L.filter (\(FunctionItem { expose }) -> isJust expose) functions
  functions' = L.filter (\(FunctionItem { custom }) -> not custom) functions

  collectionDescription = cDescription collection
  collectionFunctions = listProperty "functions" cFunctions  functions'
  collectionExposedFunctions = listProperty "exposedFunctions" cExposedFunctions exposedFunctions
  collectionSecurity = listProperty "security" cSecurity security

cDescription :: Collection -> Js.ObjectProperty
cDescription (Collection
  { name
  , required
  , properties
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
  , actions
  , individualActions
  , presets
  , temporary
  , immutable
  }
) = Js.objectProperty2 "description" (Js.object (baseDescription <> description))
  where
    description = catMaybes
      [ iconDescription
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

    baseDescription =
      [ Js.objectProperty2 "$id" (cName name)
      , Js.objectProperty2 "properties" (Js.object $ cProperties properties)
      ]

    iconDescription               = optionalProperty "icon" cIcon icon
    ownedDescription              = optionalProperty "owned" cOwned owned
    timestampsDescription         = optionalProperty "timestamps" cTimestamps timestamps
    searchDescription             = optionalProperty "search" cSearch search
    immutableDescription          = optionalProperty "immutable" cImmutable immutable
    temporaryDescription          = optionalProperty "temporary" cTemporary temporary
    layoutDescription             = optionalProperty "layout" cLayout layout
    formDescription               = listProperty "form" cNames form
    tableDescription              = listProperty "table" cNames table
    tableLayoutDescription        = listProperty "tableLayout" cTableLayout tableLayout
    formLayoutDescription         = listProperty "formLayout" cFormLayout formLayout
    actionsDescription            = listProperty "actions" cActions actions
    individualActionsDescription  = listProperty "individualActions" cActions individualActions
    filtersDescription            = listProperty "filters" cNames filters
    indexesDescription            = listProperty "indexes" cNames indexes
    presetsDescription            = listProperty "presets" cPresets presets
    writableDescription           = listProperty "writable" cNames writable
    requiredDescription           = optionalProperty "required" cRequired required
    tableMetaDescription          = listProperty "tableMeta" cNames tableMeta
    filtersPresetsDescription     = listProperty "filtersPresets" cFiltersPresets filtersPresets
    preferredDescription          = listProperty "preferred" cPreferred preferred

cImmutable :: CollectionImmutable -> Js.Tree
cImmutable (CollectionImmutableBool bool) = Js.boolean bool
cImmutable (CollectionImmutableList immutable) = cNames immutable

cIcon :: CollectionIcon -> Js.Tree
cIcon (CollectionIcon icon) = Js.string icon

cOwned :: CollectionOwned -> Js.Tree
cOwned (CollectionOwnedBoolean _ owned) = Js.boolean owned
cOwned (CollectionOwnedCustom _ owned) = Js.string owned

cTimestamps :: CollectionTimestamps -> Js.Tree
cTimestamps (CollectionTimestamps timestamps) = Js.boolean timestamps

cSearch :: CollectionSearch -> Js.Tree
cSearch (CollectionSearch { placeholder, indexes }) = Js.object (base <> properties)
  where
    base =
      [ Js.objectProperty2 "indexes" (cNames indexes)
      ]
    properties = catMaybes
      [ optionalProperty "placeholder" Js.string placeholder
      ]

cTemporary :: CollectionTemporary -> Js.Tree
cTemporary (CollectionTemporary {index, expireAfterSeconds}) =
  Js.object
    [ Js.objectProperty2 "index" (cName index)
    , Js.objectProperty2 "expireAfterSeconds" (Js.int expireAfterSeconds)
    ]

cPresets :: CollectionPresets -> Js.Tree
cPresets collectionPresets =
  L.toUnfoldable collectionPresets
  # map Js.string
  # Js.array

cPreferred :: CollectionPreferred -> Js.Tree
cPreferred preferred =
  L.toUnfoldable preferred
    # map go
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
    }) = Js.objectProperty2 role (Js.object description)
      where
        description = catMaybes
          [ optionalProperty "layout" cLayout layout
          , listProperty "tableMeta" cNames tableMeta
          , listProperty "form" cNames form
          , listProperty "table" cNames table
          , listProperty "tableLayout" cTableLayout tableLayout
          , listProperty "formLayout" cFormLayout formLayout
          , listProperty "actions" cActions actions
          , listProperty "individualActions" cActions individualActions
          , listProperty "filters" cNames filters
          , listProperty "filtersPresets" cFiltersPresets filtersPresets
          ]

cFormLayout :: CollectionFormLayout -> Js.Tree
cFormLayout collectionFormLayout =
  Js.objectProperty2 "fields" fields
    # A.singleton
    # Js.object
  where
    fields =
      map cField collectionFormLayout
        # L.toUnfoldable
        # Js.object

    cField (LayoutItem { name, component, span_, if_, verticalSpacing }) =
      Js.objectProperty2 (getName name) (Js.object layout)
      where
        layout = catMaybes
          [ optionalProperty "span" Js.float span_
          , optionalProperty "verticalSpacing" Js.float verticalSpacing
          , optionalProperty "if" (\(Cond _ expr) -> cExpr expr) if_
          , optionalProperty "component" cComponent component
          ]

    cComponent (LayoutItemComponent { name, props }) = Js.object component
      where
        component = catMaybes
          [ optionalProperty "name" Js.string name
          , optionalProperty "props" cMacro props
          ]

cActions :: CollectionActions -> Js.Tree
cActions actions =
  L.toUnfoldable actions
    # map go
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
    }) = Js.objectProperty2 (getName actionName) (Js.object actionsProperties)
      where
        actionsProperties = catMaybes
          [ optionalProperty "label" Js.string label
          , optionalProperty "icon" Js.string icon
          , optionalProperty "ask" Js.boolean ask
          , optionalProperty "selection" Js.boolean selection
          , optionalProperty "effect" Js.string effect
          , optionalProperty "translate" Js.boolean translate
          , optionalProperty "fetchItem" Js.boolean fetchItem
          , optionalProperty "button" Js.boolean button
          , optionalProperty "setItem" Js.boolean setItem
          , optionalProperty "clearItem" Js.boolean clearItem
          , optionalProperty "params" cMacro params
          , optionalProperty "query" cMacro query
          , listProperty "requires" cNames requires
          ]

cLayout :: CollectionLayout -> Js.Tree
cLayout (CollectionLayout { name, options }) = Js.object layoutProperties
  where
    layoutProperties = catMaybes
      [ optionalProperty "name" Js.string (Just name)
      , optionalProperty "options" cLayoutOptions options
      ]

    cLayoutOptions (LayoutOptions {title, badge, picture, information, active, translateBadge}) =
      Js.object layoutOptionsProperties
      where
        layoutOptionsProperties =
          catMaybes
            [ optionalProperty "title" (Js.string <<< getName) title
            , optionalProperty "badge" (Js.string <<< getName) badge
            , optionalProperty "picture" (Js.string <<< getName) picture
            , optionalProperty "information" (Js.string <<< getName) information
            , optionalProperty "active" (Js.string <<< getName) active
            , optionalProperty "translateBadge" Js.boolean translateBadge
            ]

cTableLayout :: CollectionTableLayout -> Js.Tree
cTableLayout tableLayout =
  Js.objectProperty2 "actions" actionsProperties
    # A.singleton
    # Js.object
  where
    actionsProperties =
      map go tableLayout
        # L.toUnfoldable
        # Js.object

    go (TableLayoutItem { actionName, route, if_, button, action }) =
      case route of
        Just route' -> Js.objectProperty2 route' tableLayoutProperties
        Nothing -> Js.objectProperty2 (getName actionName) tableLayoutProperties
      where
        base = catMaybes
          [ optionalProperty "if" cCond if_
          , optionalProperty "button" cButton button
          ]

        tableLayoutProperties = Js.object $
          case cActions (L.singleton action) of
            Js.Object action' ->
              case head action' of
                Just (Js.ObjectProperty2 _ (Js.Object actionProperties)) ->
                  union base actionProperties
                _ -> base
            _ -> []

    cButton (Left bool) = Js.boolean bool
    cButton (Right cond) = cCond cond

cFiltersPresets :: CollectionFiltersPresets -> Js.Tree
cFiltersPresets filtersPresets =
  map go filtersPresets
    # L.toUnfoldable
    # Js.object
  where
    go (FiltersPresetsItem { name, label, badgeFunction, filters  }) =
      Js.objectProperty2 (getName name) (Js.object filtersPresetsProperties)
      where
      filtersPresetsProperties = catMaybes
          [ optionalProperty "name" Js.string label
          , optionalProperty "badgeFunction" Js.string badgeFunction
          , optionalProperty "filters" cMacro filters
          ]

cFunctions :: CollectionFunctions -> Js.Tree
cFunctions functions =
  map (\(FunctionItem { functionName }) -> Js.objectProperty1 (getName functionName)) functions
    # L.toUnfoldable
    # Js.object

cExposedFunctions :: CollectionFunctions -> Js.Tree
cExposedFunctions exposedFunctions =
  map go exposedFunctions
    # L.toUnfoldable
    # Js.object
  where
    go (FunctionItem { functionName, expose }) =
      let functionName' = getName functionName
      in case expose of
        Just (Attribute _ _ attributeValue) ->
          case attributeValue of
            ALiteral _ literal -> Js.objectProperty2 functionName' (cLiteral literal)
            _ -> Js.objectProperty1 functionName'
        Nothing -> Js.objectProperty1 functionName'

cSecurity :: CollectionSecurity -> Js.Tree
cSecurity secutiry =
  Js.object
    [ Js.objectProperty2 "functions"
      $ map go secutiry
        # L.toUnfoldable
        # Js.object
    ]
  where
    go (SecurityItem { functionName, rateLimiting, logging }) =
      Js.objectProperty2 (getName functionName) (Js.object secutiryProperties)
      where
        secutiryProperties =  catMaybes
          [ optionalProperty "rateLimiting" (Js.object <<< cSecurityRateLimiting) rateLimiting
          , optionalProperty "logging" (Js.object <<< cSecurityLogging) logging
          ]

    cSecurityRateLimiting (SecurityRateLimiting { strategy, scale }) =
      catMaybes
        [ optionalProperty "strategy" Js.string strategy
        , optionalProperty "scale" Js.int scale
        ]

    cSecurityLogging (SecurityLogging { strategy }) =
      catMaybes
        [ optionalProperty "strategy" Js.string strategy
        ]

cRequired :: CollectionRequired -> Js.Tree
cRequired required =
  if hasCondition then
    Js.array $ L.toUnfoldable $ map getPropertyName required
  else
    Js.object $ L.toUnfoldable $ map getPropertyWithCondition required
  where
    hasCondition :: Boolean
    hasCondition = L.all (\(Required _ _ cond) -> isNothing cond) required

    getPropertyName :: Required -> Js.Tree
    getPropertyName (Required _ propertyName _) = cName propertyName

    getPropertyWithCondition :: Required -> Js.ObjectProperty
    getPropertyWithCondition (Required _ propertyName expr) =
      Js.objectProperty2 (getName propertyName) (cRequiredProperties expr)

    cRequiredProperties :: Maybe Cond -> Js.Tree
    cRequiredProperties Nothing = Js.boolean true
    cRequiredProperties (Just cond) = cCond cond

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
cExpr (ELt e1 e2) = cBinaryExpr "lt" (cExpr e1) (cExpr e2)
cExpr (EGt e1 e2) = cBinaryExpr "gt" (cExpr e1) (cExpr e2)
cExpr (ELte e1 e2) = cBinaryExpr "lte" (cExpr e1) (cExpr e2)
cExpr (EGte e1 e2) = cBinaryExpr "gte" (cExpr e1) (cExpr e2)
cExpr (EEq e1 e2) = cBinaryExpr "equal" (cExpr e1) (cExpr e2)
cExpr (EIn e1 e2) = cBinaryExpr "in" (cExpr e1) (cExpr e2)
cExpr (ENot e1) = cUnaryExpr "not" (cExpr e1)
cExpr (ETruthy e1) = cUnaryExpr "truthy" (cExpr e1)
cExpr (EOr e1 e2) =
  A.singleton (Js.objectProperty2 "or" (Js.array [cExpr e1, cExpr e2]))
    # Js.object
cExpr (EAnd e1 e2) =
  A.singleton (Js.objectProperty2 "and" (Js.array [cExpr e1, cExpr e2]))
    # Js.object

cCond :: Cond -> Js.Tree
cCond (Cond _ expr) = cExpr expr

cMacro :: Macro -> Js.Tree
cMacro (Macro _ code) = Js.raw code

cProperties :: CollectionProperties -> Array Js.ObjectProperty
cProperties properties = L.toUnfoldable (map go properties)
  where
    go :: Property -> Js.ObjectProperty
    go property@(Property { name })
      = Js.objectProperty2 (getName name) (cProperty property)

    cProperty :: Property -> Js.Tree
    cProperty (Property { type_, attributes }) = Js.object properties'
      where
        properties' = A.concat
          [ cPropertyType type_
          , cAttributes type_ attributes
          ]

    cPropertyType :: PropertyType -> Array Js.ObjectProperty
    cPropertyType propertyType =
      case propertyType of
        PEnum _ -> []
        PConst _ -> []
        PNum _ -> A.singleton (cType "number")
        PInteger _ -> A.singleton (cType "integer")
        PString _ -> A.singleton (cType "string")
        PBoolean _ -> A.singleton (cType "boolean")
        PArray _ _ -> A.singleton (cType "array")
        PRef _ collectionName ->
          A.singleton (Js.objectProperty2 "$ref" (cName collectionName))
        PObject _ required properties' additionalProperties ->
          catMaybes
            [ Just (cType "object")
            , optionalProperty "required" cRequired required
            , optionalProperty "properties" (Js.object <<< cProperties) (Just properties')
            , optionalProperty "additionalProperties" cAdditionalProperties additionalProperties
            ]
      where
        cType type_ = Js.objectProperty2 "type" (Js.string type_)
        cAdditionalProperties (AdditionalProperties additionalProperties) = Js.object (cPropertyType additionalProperties)

    cAttributes :: PropertyType -> Attributes -> Array Js.ObjectProperty
    cAttributes propertyType attributes =
      case propertyType of
        PConst _ -> cConstAttribute attributes
        PEnum _ -> cEnumAttribute attributes
        PArray _ type_ -> cArrayAttribute type_ attributes
        _ -> L.toUnfoldable (map cAttribute attributes)
      where
        findAttribute :: Attributes -> String -> Maybe Attribute
        findAttribute attributes' attributeName =
          L.find (\(Attribute _ attributeName' _) -> getName attributeName' == attributeName) attributes'

        cAttribute :: Attribute -> Js.ObjectProperty
        cAttribute (Attribute _ attributeName attributeValue) =
          let attributeName' = getName attributeName
          in case attributeValue of
            ALiteral _ literal -> Js.objectProperty2 attributeName' (cLiteral literal)
            AExpr _ expr -> Js.objectProperty2 attributeName' (cExpr expr)

        cArrayAttribute :: PropertyType -> Attributes -> Array Js.ObjectProperty
        cArrayAttribute type_ attributes' = A.union [typeAttributes'] arrayAttributes''
          where
            typeAttributes = L.filter (\(Attribute _ attributeName _) -> not (getName attributeName `elem` arrayAttributes)) attributes
            arrayAttributes' = L.filter (\(Attribute _ attributeName _) -> getName attributeName `elem` arrayAttributes) attributes'

            typeAttributes' =
              concat
                [ cPropertyType type_
                , cAttributes type_ typeAttributes
                ]
                # Js.object
                # Js.objectProperty2 "items"

            arrayAttributes'' = L.toUnfoldable (map cAttribute arrayAttributes')

        cEnumAttribute :: Attributes -> Array Js.ObjectProperty
        cEnumAttribute attributes' =
            let values = findAttribute attributes' "values"
            in case values of
              Just (Attribute _ _ (ALiteral _ array@(LArray _ _))) ->
                [ Js.objectProperty2 "enum" (cLiteral array)
                ]
              _ -> []

        cConstAttribute :: Attributes -> Array Js.ObjectProperty
        cConstAttribute attributes' =
            let value = findAttribute attributes' "value"
            in case value of
              Just (Attribute _ _ (ALiteral _ literal)) ->
                [ Js.objectProperty2 "const" (cLiteral literal)
                ]
              _ -> []

    arrayAttributes =
      [ "default"
      , "minItems"
      , "maxItems"
      , "uniqueItems"
      ]

cLiteral :: Literal -> Js.Tree
cLiteral =
  fix \self -> case _ of
    LNull _ -> Js.null
    LUndefined _ -> Js.undefined
    LInteger _ value -> Js.int value
    LNum _ value -> Js.float value
    LString _ value -> Js.string value
    LBoolean _ value -> Js.boolean value
    LArray _ value ->
      map self value
        # L.toUnfoldable
        # Js.array
    LProperty _ propertyName -> cName propertyName

cName :: forall n. Name n => n -> Js.Tree
cName name =
  name
    # getName
    # Js.string

cNames :: forall a. Name a => L.List a -> Js.Tree
cNames xs =
  map cName xs
    # L.toUnfoldable
    # Js.array

listProperty' :: forall a. String -> (L.List a -> Js.Tree) -> L.List a -> Maybe Js.ObjectProperty
listProperty' k f x =
  case x of
    L.Nil -> Just $ Js.objectProperty2 k (Js.array [])
    _ -> Just $ Js.objectProperty2 k (f x)

listProperty :: forall a. String -> (L.List a -> Js.Tree) -> L.List a -> Maybe Js.ObjectProperty
listProperty k f x =
  case x of
    L.Nil -> Nothing
    _ -> Just (Js.objectProperty2 k (f x))

optionalProperty :: forall a. String -> (a -> Js.Tree) -> Maybe a -> Maybe Js.ObjectProperty
optionalProperty k f x =
  case x of
    Just a -> Just (Js.objectProperty2 k (f a))
    Nothing -> Nothing
