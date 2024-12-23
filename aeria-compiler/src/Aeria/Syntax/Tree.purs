module Aeria.Syntax.Tree where

import Prelude

import Aeria.Diagnostic.Position (Span, ghostSpan)
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String.Utils (ucLower)
import Yoga.JSON (class WriteForeign, writeImpl)

type Ident
  = String

class Name a where
  getName :: a -> Ident
  getSpan :: a -> Span

data FunctionName = FunctionName Span Ident

instance funcName :: Name FunctionName where
  getName (FunctionName _ name) = name
  getSpan (FunctionName span _) = span

derive instance genericFunctionName :: Generic FunctionName _

instance showFunctionName :: Show FunctionName where
  show = genericShow

instance eqFunctionName :: Eq FunctionName where
  eq = genericEq

instance ordFunctionName :: Ord FunctionName where
  compare = genericCompare

instance WriteForeign FunctionName
  where
  writeImpl (FunctionName _ functionName) =
    writeImpl
      { kind: "FunctionName"
      , value: writeImpl functionName
      }

data CollectionName = CollectionName Span Ident

instance colName :: Name CollectionName where
  getName (CollectionName _ name) = ucLower name
  getSpan (CollectionName span _) = span

derive instance genericCollectionName :: Generic CollectionName _

instance showCollectionName :: Show CollectionName where
  show = genericShow

instance eqCollectionName :: Eq CollectionName where
  eq = genericEq

instance ordCollectionName :: Ord CollectionName where
  compare = genericCompare

instance WriteForeign CollectionName
  where
  writeImpl (CollectionName _ collectionName) =
    writeImpl
      { kind: "CollectionName"
      , value: writeImpl collectionName
      }

data PropertyName = PropertyName Span Ident

instance propName :: Name PropertyName where
  getName (PropertyName _ name) = name
  getSpan (PropertyName span _) = span

derive instance genericPropertyName :: Generic PropertyName _

instance showPropertyName :: Show PropertyName where
  show = genericShow

instance eqPropertyName :: Eq PropertyName where
  eq = genericEq

instance ordPropertyName :: Ord PropertyName where
  compare = genericCompare

instance WriteForeign PropertyName
  where
  writeImpl (PropertyName _ propertyName) =
    writeImpl
    { kind: "PropertyName"
    , value: writeImpl propertyName
    }

data AttributeName = AttributeName Span Ident

instance attrName :: Name AttributeName where
  getName (AttributeName _ name) = name
  getSpan (AttributeName span _) = span

derive instance genericAttributeName :: Generic AttributeName _

instance showAttributeName :: Show AttributeName where
  show = genericShow

instance eqAttributeName :: Eq AttributeName where
  eq = genericEq

instance WriteForeign AttributeName
  where
  writeImpl (AttributeName _ attributeName) =
    writeImpl
    { kind: "AttributeName"
    , value: writeImpl attributeName
    }

data ExtendsName = ExtendsName Ident Ident  -- TODO: add span

instance extName :: Name ExtendsName where
  getName (ExtendsName _ name) = name
  getSpan (ExtendsName _ _) = ghostSpan

derive instance genericExtendsName :: Generic ExtendsName _

instance showExtendsName :: Show ExtendsName where
  show = genericShow

instance eqExtendsName :: Eq ExtendsName where
  eq = genericEq

instance WriteForeign ExtendsName
  where
  writeImpl (ExtendsName package collection) =
    writeImpl
    { kind: "ExtendsName"
    , package: writeImpl package
    , collection: writeImpl collection
    }

data Literal
  = LNull Span
  | LUndefined Span
  | LInteger Span Int
  | LNum Span Number
  | LString Span String
  | LBoolean Span Boolean
  | LProperty Span PropertyName
  | LArray Span (List Literal)

derive instance genericLiteral :: Generic Literal _

instance showLiteral :: Show Literal where
  show (LNull _) = "null"
  show (LUndefined _) = "undefined"
  show (LInteger _ _) = "integer"
  show (LNum _ _) = "num"
  show (LString _ _) = "string"
  show (LBoolean _ _) = "boolean"
  show (LProperty _ _) = "property name"
  show (LArray _ _) = "array"

instance eqLiteral :: Eq Literal where
  eq x = genericEq x

instance WriteForeign Literal
  where
  writeImpl (LNull _) =
    writeImpl
    { kind: "LNull"
    }
  writeImpl (LUndefined _) =
    writeImpl
    { kind: "LUndefined"
    }
  writeImpl (LInteger _ x) =
    writeImpl
    { kind: "LInteger"
    , value: x
    }
  writeImpl (LNum _ x) =
    writeImpl
    { kind: "LNum"
    , value: x
    }
  writeImpl (LString _ x) =
    writeImpl
    { kind: "LString"
    , value: x
    }
  writeImpl (LBoolean _ x) =
    writeImpl
    { kind: "LBoolean"
    , value: x
    }
  writeImpl (LProperty _ x) =
    writeImpl
    { kind: "LProperty"
    , value: writeImpl x
    }
  writeImpl (LArray _ arr) =
    writeImpl { kind: "LArray"
    , value: writeImpl (toUnfoldable arr :: Array Literal)
    }

data Expr
  = ELiteral Literal
  | ELte Expr Expr
  | EGte Expr Expr
  | EAnd Expr Expr
  | EIn Expr Expr
  | ELt Expr Expr
  | EGt Expr Expr
  | EEq Expr Expr
  | EOr Expr Expr
  | ETruthy Expr
  | ENot Expr
  -- | EExists Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance eqExpr :: Eq Expr where
  eq x = genericEq x

instance WriteForeign Expr
  where
  writeImpl (ELiteral e1) =
    writeImpl
    { kind: "ELiteral"
    , expr: writeImpl e1
    }
  writeImpl (ELte e1 e2) =
    writeImpl
    { kind: "ELte"
    , left: writeImpl e1
    , right: writeImpl e2
    }
  writeImpl (EGte e1 e2) =
    writeImpl
    { kind: "EGte"
    , left: writeImpl e1
    , right: writeImpl e2
    }
  writeImpl (EAnd e1 e2) =
    writeImpl
    { kind: "EAnd"
    , left: writeImpl e1
    , right: writeImpl e2
    }
  writeImpl (EIn e1 e2) =
    writeImpl
    { kind: "EIn"
    , left: writeImpl e1
    , right: writeImpl e2
    }
  writeImpl (ELt e1 e2) =
    writeImpl
    { kind: "ELt"
    , left: writeImpl e1, right: writeImpl e2
    }
  writeImpl (EGt e1 e2) =
    writeImpl
    { kind: "EGt"
    , left: writeImpl e1, right: writeImpl e2
    }
  writeImpl (EEq e1 e2) =
    writeImpl
    { kind: "EEq"
    , left: writeImpl e1, right: writeImpl e2
    }
  writeImpl (EOr e1 e2) =
    writeImpl
    { kind: "EOr"
    , left: writeImpl e1, right: writeImpl e2
    }
  -- writeImpl (EExists e1) =
  --   writeImpl
  --   { kind: "EExists"
  --   , expr: writeImpl e1
  --   }
  writeImpl (ENot e1) =
    writeImpl
    { kind: "ENot"
    , expr: writeImpl e1
    }
  writeImpl (ETruthy e1) =
    writeImpl
    { kind: "ETruthy"
    , expr: writeImpl e1
    }

data Cond = Cond Span Expr

derive instance genericCond :: Generic Cond _

instance showCond :: Show Cond where
  show x = genericShow x

instance eqCond :: Eq Cond where
  eq x = genericEq x

instance WriteForeign Cond
  where
  writeImpl (Cond _ expr) =
    writeImpl
      { kind: "Cond"
      , expr: writeImpl expr
      }

data PropertyType
  = PEnum Span
  | PNum Span
  | PString Span
  | PInteger Span
  | PBoolean Span
  | PConst Span
  | PRef Span CollectionName
  | PArray Span PropertyType
  | PObject Span (Maybe CollectionRequired) CollectionProperties (Maybe AdditionalProperties)

derive instance genericPropertyType :: Generic PropertyType _

instance showPropertyType :: Show PropertyType where
  show x = genericShow x

instance eqPropertyType :: Eq PropertyType where
  eq x = genericEq x

instance WriteForeign PropertyType
  where
  writeImpl (PEnum _) =
    writeImpl
    { kind: "PEnum"
    }
  writeImpl (PNum _) =
    writeImpl
    { kind: "PNum"
    }
  writeImpl (PString _) =
    writeImpl
    { kind: "PString"
    }
  writeImpl (PInteger _) =
    writeImpl
    { kind: "PInteger"
    }
  writeImpl (PBoolean _) =
    writeImpl
    { kind: "PBoolean"
    }
  writeImpl (PRef _ ref) =
    writeImpl
    { kind: "PRef",
      ref
    }
  writeImpl (PArray _ arr) =
    writeImpl
    { kind: "PArray"
    , type: writeImpl arr
    }
  writeImpl (PConst _) =
    writeImpl
    { kind: "PConst"
    }
  writeImpl (PObject _ required properties _) =
    writeImpl
    { kind: "PObject"
    , required: writeImpl (map (toUnfoldable :: _ -> Array Required) required)
    , properties: writeImpl (toUnfoldable properties :: Array Property)
    }

type CollectionRequired
  = List Required

data Required
  = Required Span PropertyName (Maybe Cond)

derive instance genericRequired :: Generic Required _

instance showRequired :: Show Required where
  show = genericShow

instance eqRequired :: Eq Required where
  eq = genericEq

instance WriteForeign Required
  where
  writeImpl (Required _ name cond) =
    writeImpl
      { kind: "Required"
      , name: writeImpl name
      , cond: writeImpl cond
      }

type Attributes
  = List Attribute

data Attribute
  = Attribute Span AttributeName AttributeValue

derive instance genericAttribute :: Generic Attribute _

instance showAttribute :: Show Attribute where
  show = genericShow

instance eqAttribute :: Eq Attribute where
  eq = genericEq

instance WriteForeign Attribute
  where
  writeImpl (Attribute _ attributeName attributeValue) =
    writeImpl
      { kind: "Attribute"
      , attributeName: writeImpl attributeName
      , attributeValue: writeImpl attributeValue
      }

data AttributeValue
  = ALiteral Span Literal
  | AExpr Span Expr

derive instance genericAttributeValue :: Generic AttributeValue _

instance showAttributeValue :: Show AttributeValue where
  show = genericShow

instance eqAttributeValue :: Eq AttributeValue where
  eq = genericEq

instance WriteForeign AttributeValue
  where
  writeImpl (ALiteral _ literal) =
    writeImpl
      { kind: "ALiteral"
      , literal: writeImpl literal
      }
  writeImpl (AExpr _ expr) =
    writeImpl
      { kind: "AExpr"
      , expr: writeImpl expr
      }

type CollectionProperties
  = List Property

data Property
  = Property
    { span :: Span
    , name :: PropertyName
    , type_ :: PropertyType
    , attributes :: Attributes
    }

instance WriteForeign Property where
  writeImpl (Property { name, type_, attributes }) =
    writeImpl
      { kind: "Property"
      , name: writeImpl name
      , type: writeImpl type_
      , attributes: writeImpl (toUnfoldable attributes :: Array Attribute)
      }

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

instance eqProperty :: Eq Property where
  eq = genericEq

data Macro
  = Macro Span String

derive instance genericMacro :: Generic Macro _

instance showMacro :: Show Macro where
  show = genericShow

instance eqMacro :: Eq Macro where
  eq = genericEq

instance WriteForeign Macro where
  writeImpl (Macro _ code) =
    writeImpl
      { kind: "Macro"
      , macro: writeImpl code
      }

type CollectionTable = List PropertyName

type CollectionTableMeta = List PropertyName

type CollectionForm = List PropertyName

type CollectionFilters = List PropertyName

type CollectionIndexes = List PropertyName

data CollectionSearch = CollectionSearch
  { placeholder :: Maybe String,
    indexes :: List PropertyName
  }

derive instance genericCollectionSearch :: Generic CollectionSearch _

instance showCollectionSearch :: Show CollectionSearch where
  show = genericShow

instance eqCollectionSearch :: Eq CollectionSearch where
  eq = genericEq

instance WriteForeign CollectionSearch where
  writeImpl (CollectionSearch { placeholder, indexes}) =
    writeImpl
      { kind: "CollectionSearch"
      , placeholder: writeImpl placeholder
      , indexes:  writeImpl (toUnfoldable indexes :: Array PropertyName)
      }

data CollectionIcon = CollectionIcon String

derive instance genericCollectionIcon :: Generic CollectionIcon _

instance showCollectionIcon :: Show CollectionIcon where
  show = genericShow

instance eqCollectionIcon :: Eq CollectionIcon where
  eq = genericEq

instance WriteForeign CollectionIcon where
  writeImpl (CollectionIcon icon) =
    writeImpl
      { kind: "CollectionIcon"
      , icon: writeImpl icon
      }

data CollectionOwned
  = CollectionOwnedBoolean Span Boolean
  | CollectionOwnedCustom Span String

derive instance genericCollectionOwned :: Generic CollectionOwned _

instance showCollectionOwned :: Show CollectionOwned where
  show = genericShow

instance eqCollectionOwned :: Eq CollectionOwned where
  eq = genericEq

instance WriteForeign CollectionOwned where
  writeImpl (CollectionOwnedBoolean _ owned) =
    writeImpl
      { kind: "CollectionOwnedBoolean"
      , owned: writeImpl owned
      }
  writeImpl (CollectionOwnedCustom _ owned) =
    writeImpl
      { kind: "CollectionOwnedCustom"
      , owned: writeImpl owned
      }

data CollectionTimestamps = CollectionTimestamps Boolean

derive instance genericCollectionTimestamps :: Generic CollectionTimestamps _

instance showCollectionTimestamps :: Show CollectionTimestamps where
  show = genericShow

instance eqCollectionTimestamps :: Eq CollectionTimestamps where
  eq = genericEq

instance WriteForeign CollectionTimestamps where
  writeImpl (CollectionTimestamps timestamps) =
    writeImpl
      { kind: "CollectionTimestamps"
      , timestamps: writeImpl timestamps
      }

type CollectionFiltersPresets = List FiltersPresetsItem

data FiltersPresetsItem = FiltersPresetsItem
  { span :: Span
  , name :: PropertyName
  , label :: Maybe String
  , badgeFunction :: Maybe String
  , filters :: Maybe Macro
  }

derive instance genericFiltersPresetsItem :: Generic FiltersPresetsItem _

instance showFiltersPresetsItem :: Show FiltersPresetsItem where
  show = genericShow

instance eqFiltersPresetsItem :: Eq FiltersPresetsItem where
  eq = genericEq

instance WriteForeign FiltersPresetsItem where
  writeImpl (FiltersPresetsItem { name, label, badgeFunction, filters }) =
    writeImpl
      { kind: "FiltersPresetsItem"
      , name: writeImpl name
      , label: writeImpl label
      , badgeFunction: writeImpl badgeFunction
      , filters: writeImpl filters
      }

data CollectionLayout = CollectionLayout
  { span :: Span
  , name :: String
  , options :: Maybe LayoutOptions
  }

derive instance genericCollectionLayout :: Generic CollectionLayout _

instance showCollectionLayout :: Show CollectionLayout where
  show = genericShow

instance eqCollectionLayout :: Eq CollectionLayout where
  eq = genericEq

instance WriteForeign CollectionLayout where
  writeImpl (CollectionLayout {name, options}) =
    writeImpl
      { kind: "CollectionLayout"
      , name: writeImpl name
      , options: writeImpl options
      }

data LayoutOptions = LayoutOptions
  { title :: Maybe PropertyName
  , badge :: Maybe PropertyName
  , picture :: Maybe PropertyName
  , information :: Maybe PropertyName
  , active :: Maybe PropertyName
  , translateBadge :: Maybe Boolean
  }

derive instance genericLayoutOptions :: Generic LayoutOptions _

instance showLayoutOptions :: Show LayoutOptions where
  show = genericShow

instance eqLayoutOptions :: Eq LayoutOptions where
  eq = genericEq

instance WriteForeign LayoutOptions where
  writeImpl (LayoutOptions {title, badge, picture, information, translateBadge, active}) =
    writeImpl
      { kind: "LayoutOptions"
      , title: writeImpl title
      , badge: writeImpl badge
      , picture: writeImpl picture
      , information: writeImpl information
      , active: writeImpl active
      , translateBadge: writeImpl translateBadge
      }

data LayoutItemCondition = LayoutItemCondition PropertyName Cond

derive instance genericLayoutItemCondition :: Generic LayoutItemCondition _

instance showLayoutItemCondition :: Show LayoutItemCondition where
  show = genericShow

instance eqLayoutItemCondition :: Eq LayoutItemCondition where
  eq = genericEq

instance WriteForeign LayoutItemCondition where
  writeImpl (LayoutItemCondition propertyName cond) =
    writeImpl
      { kind: "LayoutItemCondition"
      , propertyName: writeImpl propertyName
      , cond: writeImpl cond
      }

data LayoutItemComponent = LayoutItemComponent
  { span :: Span
  , name :: Maybe String
  , props :: Maybe Macro
  }

derive instance genericLayoutItemComponent :: Generic LayoutItemComponent _

instance showLayoutItemComponent :: Show LayoutItemComponent where
  show = genericShow

instance eqLayoutItemComponent :: Eq LayoutItemComponent where
  eq = genericEq

instance WriteForeign LayoutItemComponent where
  writeImpl (LayoutItemComponent { name, props }) =
    writeImpl
      { kind: "LayoutItemComponent"
      , name: writeImpl name
      , props: writeImpl props
      }

data CollectionImmutable
  = CollectionImmutableBool Boolean
  | CollectionImmutableList (List PropertyName)

derive instance genericCollectionImmutable :: Generic CollectionImmutable _

instance showCollectionImmutable :: Show CollectionImmutable where
  show = genericShow

instance eqCollectionImmutable :: Eq CollectionImmutable where
  eq = genericEq

instance WriteForeign CollectionImmutable where
  writeImpl (CollectionImmutableBool immutable) =
    writeImpl
      { kind: "CollectionImmutableBool"
      , immutable: writeImpl immutable
      }
  writeImpl (CollectionImmutableList immutables) =
    writeImpl
      { kind: "CollectionImmutableList"
      , immutables: writeImpl (toUnfoldable immutables :: Array PropertyName)
      }

type CollectionWritable = List PropertyName

type CollectionFunctions = List FunctionItem

data FunctionItem =
  FunctionItem
    { span :: Span
    , functionName :: FunctionName
    , custom :: Boolean
    , expose :: Maybe Attribute
    }

derive instance genericFunctionItem :: Generic FunctionItem _

instance showFunctionItem :: Show FunctionItem where
  show = genericShow

instance eqFunctionItem :: Eq FunctionItem where
  eq = genericEq

instance WriteForeign FunctionItem where
  writeImpl (FunctionItem { functionName, custom, expose }) =
    writeImpl
      { kind: "FunctionItem"
      , functioName: writeImpl functionName
      , custom: writeImpl custom
      , expose: writeImpl expose
      }

type CollectionSecurity = List SecurityItem

data SecurityItem = SecurityItem
  { span :: Span
  , functionName :: FunctionName
  , rateLimiting :: Maybe SecurityRateLimiting
  , logging :: Maybe SecurityLogging
  }

derive instance genericSecurityItem :: Generic SecurityItem _

instance showSecurityItem :: Show SecurityItem where
  show = genericShow

instance eqSecurityItem :: Eq SecurityItem where
  eq = genericEq

instance WriteForeign SecurityItem where
  writeImpl (SecurityItem { functionName, rateLimiting, logging}) =
    writeImpl
      { kind: "SecurityItem"
      , functionName: writeImpl functionName
      , rateLimiting: writeImpl rateLimiting
      , logging: writeImpl logging
      }

data SecurityRateLimiting = SecurityRateLimiting
  { span :: Span
  , strategy :: Maybe String
  , scale :: Maybe Int
  }

derive instance genericSecurityRateLimiting :: Generic SecurityRateLimiting _

instance showSecurityRateLimiting :: Show SecurityRateLimiting where
  show = genericShow

instance eqSecurityRateLimiting :: Eq SecurityRateLimiting where
  eq = genericEq

instance WriteForeign SecurityRateLimiting where
  writeImpl (SecurityRateLimiting { strategy, scale }) =
    writeImpl
      { kind: "SecurityRateLimiting"
      , strategy: writeImpl strategy
      , scale: writeImpl scale
      }

data SecurityLogging = SecurityLogging
  { span :: Span
  , strategy :: Maybe String
  }

derive instance genericSecurityLogging :: Generic SecurityLogging _

instance showSecurityLogging :: Show SecurityLogging where
  show = genericShow

instance eqSecurityLogging :: Eq SecurityLogging where
  eq = genericEq

instance WriteForeign SecurityLogging where
  writeImpl (SecurityLogging { strategy }) =
    writeImpl
      { kind: "SecurityLogging"
      , strategy: writeImpl strategy
      }

type CollectionPresets = List String

type CollectionActions = List ActionItem

data ActionItem = ActionItem
  { span :: Span
  , actionName :: PropertyName
  , label :: Maybe String
  , icon :: Maybe String
  , ask :: Maybe Boolean
  , selection :: Maybe Boolean
  , effect :: Maybe String
  , button :: Maybe Boolean
  , translate :: Maybe Boolean
  , setItem :: Maybe Boolean
  , fetchItem :: Maybe Boolean
  , clearItem :: Maybe Boolean
  , params :: Maybe Macro
  , query :: Maybe Macro
  , requires :: List PropertyName
  }

derive instance genericActionItem :: Generic ActionItem _

instance showActionItem :: Show ActionItem where
  show = genericShow

instance eqActionItem :: Eq ActionItem where
  eq = genericEq

instance WriteForeign ActionItem where
  writeImpl (ActionItem
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
    writeImpl
      { kind: "ActionItem"
      , actionName: writeImpl actionName
      , label: writeImpl label
      , icon: writeImpl icon
      , ask: writeImpl ask
      , selection: writeImpl selection
      , effect: writeImpl effect
      , button: writeImpl button
      , translate: writeImpl translate
      , setItem: writeImpl setItem
      , fetchItem: writeImpl fetchItem
      , clearItem: writeImpl clearItem
      , params: writeImpl params
      , query: writeImpl query
      , requires: writeImpl (toUnfoldable requires :: Array PropertyName)
      }

type CollectionIndividualActions = List ActionItem

data CollectionTemporary =
  CollectionTemporary
    { index :: PropertyName
    , expireAfterSeconds :: Int
    }

derive instance genericCollectionTemporary :: Generic CollectionTemporary _

instance showCollectionTemporary :: Show CollectionTemporary where
  show = genericShow

instance eqCollectionTemporary :: Eq CollectionTemporary where
  eq = genericEq

instance WriteForeign CollectionTemporary where
  writeImpl (CollectionTemporary { index, expireAfterSeconds }) =
    writeImpl
      { kind: "CollectionTemporary"
      , index: index
      , expireAfterSeconds: writeImpl expireAfterSeconds
      }

type CollectionFormLayout = List LayoutItem

data LayoutItem = LayoutItem
  { span :: Span
  , name :: PropertyName
  , verticalSpacing :: Maybe Number
  , separator :: Maybe String
  , span_ :: Maybe Number
  , component :: Maybe LayoutItemComponent
  , if_ :: Maybe Cond
  }

derive instance genericLayoutItem :: Generic LayoutItem _

instance showLayoutItem :: Show LayoutItem where
  show = genericShow

instance eqLayoutItem :: Eq LayoutItem where
  eq = genericEq

instance WriteForeign LayoutItem where
  writeImpl (LayoutItem { name, verticalSpacing, separator, span_, component, if_ }) =
    writeImpl
      { kind: "LayoutItem"
      , name: writeImpl name
      , verticalSpacing: writeImpl verticalSpacing
      , separator: writeImpl separator
      , span_: writeImpl span_
      , component: writeImpl component
      , if_: writeImpl if_
      }

type CollectionTableLayout = List TableLayoutItem

data TableLayoutItem = TableLayoutItem
  { span :: Span
  , actionName :: PropertyName
  , route :: Maybe String
  , button :: Maybe (Either Boolean Cond)
  , if_ :: Maybe Cond
  , action :: ActionItem
  }

derive instance genericTableLayoutItem :: Generic TableLayoutItem _

instance showTableLayoutItem :: Show TableLayoutItem where
  show = genericShow

instance eqTableLayoutItem :: Eq TableLayoutItem where
  eq = genericEq

instance WriteForeign TableLayoutItem where
  writeImpl (TableLayoutItem { actionName, route, button, if_, action }) =
    writeImpl
      { kind: "TableLayoutItem"
      , actionName: writeImpl actionName
      , route: writeImpl route
      , button: writeImpl button
      , if_: writeImpl if_
      , action: writeImpl action
      }

type CollectionPreferred = List PreferredItem

data PreferredItem = PreferredItem
  { span :: Span
  , role :: String
  , tableMeta :: CollectionTableMeta
  , actions :: CollectionActions
  , individualActions :: CollectionIndividualActions
  , filters :: CollectionFilters
  , filtersPresets :: CollectionFiltersPresets
  , layout :: Maybe CollectionLayout
  , table :: CollectionTable
  , form :: CollectionForm
  , tableLayout :: CollectionTableLayout
  , formLayout :: CollectionFormLayout
  }

derive instance genericPreferredItem :: Generic PreferredItem _

instance showPreferredItem :: Show PreferredItem where
  show = genericShow

instance eqPreferredItem :: Eq PreferredItem where
  eq = genericEq

instance WriteForeign PreferredItem where
  writeImpl (PreferredItem
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
    writeImpl
      { kind: "PreferredItem"
      , role: writeImpl role
      , tableMeta: writeImpl (toUnfoldable tableMeta :: Array PropertyName)
      , actions: writeImpl (toUnfoldable actions :: Array ActionItem)
      , individualActions: writeImpl (toUnfoldable individualActions :: Array ActionItem)
      , filters: writeImpl (toUnfoldable filters :: Array PropertyName)
      , filtersPresets: writeImpl (toUnfoldable filtersPresets :: Array FiltersPresetsItem)
      , layout: writeImpl layout
      , form: writeImpl (toUnfoldable form :: Array PropertyName)
      , table: writeImpl (toUnfoldable table :: Array PropertyName)
      , formLayout: writeImpl (toUnfoldable formLayout :: Array LayoutItem)
      , tableLayout: writeImpl (toUnfoldable tableLayout :: Array TableLayoutItem)
      }

data AdditionalProperties = AdditionalProperties PropertyType

derive instance genericAdditionalProperties :: Generic AdditionalProperties _

instance showAdditionalProperties :: Show AdditionalProperties where
  show = genericShow

instance eqAdditionalProperties :: Eq AdditionalProperties where
  eq = genericEq

instance WriteForeign AdditionalProperties where
  writeImpl (AdditionalProperties _) =
    writeImpl
      { kind: "AdditionalProperties"
      }

data Collection
  = Collection
    { span :: Span
    , name :: CollectionName
    , extends :: Maybe ExtendsName
    , icon :: Maybe CollectionIcon
    , owned :: Maybe CollectionOwned
    , layout :: Maybe CollectionLayout
    , search :: Maybe CollectionSearch
    , timestamps :: Maybe CollectionTimestamps
    , immutable :: Maybe CollectionImmutable
    , temporary :: Maybe CollectionTemporary
    , preferred :: CollectionPreferred
    , presets :: CollectionPresets
    , writable :: CollectionWritable
    , tableLayout :: CollectionTableLayout
    , functions :: CollectionFunctions
    , actions :: CollectionActions
    , individualActions :: CollectionIndividualActions
    , security :: CollectionSecurity
    , properties :: CollectionProperties
    , required :: Maybe CollectionRequired
    , table :: CollectionTable
    , tableMeta :: CollectionTableMeta
    , form :: CollectionForm
    , filters :: CollectionFilters
    , filtersPresets :: CollectionFiltersPresets
    , indexes :: CollectionIndexes
    , formLayout :: CollectionFormLayout
    }

derive instance genericCollection :: Generic Collection _

instance showCollection :: Show Collection where
  show = genericShow

instance eqCollection :: Eq Collection where
  eq = genericEq

instance WriteForeign Collection where
  writeImpl (Collection
    { name
    , icon
    , owned
    , timestamps
    , immutable
    , temporary
    , preferred
    , presets
    , writable
    , tableLayout
    , functions
    , actions
    , individualActions
    , security
    , properties
    , required
    , table
    , tableMeta
    , form
    , filters
    , filtersPresets
    , indexes
    , layout
    , formLayout
    , search
    }) =
      writeImpl
        { kind: "Collection"
        , name: writeImpl name
        , icon: writeImpl icon
        , owned: writeImpl owned
        , timestamps: writeImpl timestamps
        , immutable: writeImpl immutable
        , temporary: writeImpl temporary
        , search: writeImpl search
        , preferred: writeImpl (toUnfoldable preferred :: Array PreferredItem)
        , presets: writeImpl (toUnfoldable presets :: Array String)
        , writable: writeImpl (toUnfoldable writable :: Array PropertyName)
        , tableLayout: writeImpl (toUnfoldable tableLayout :: Array TableLayoutItem)
        , functions: writeImpl (toUnfoldable functions :: Array FunctionItem)
        , actions: writeImpl (toUnfoldable actions :: Array ActionItem)
        , individualActions: writeImpl (toUnfoldable individualActions :: Array ActionItem)
        , security: writeImpl (toUnfoldable security :: Array SecurityItem)
        , properties: writeImpl (toUnfoldable properties :: Array Property)
        , required: writeImpl (map (toUnfoldable :: _ -> Array Required) required)
        , table: writeImpl (toUnfoldable table :: Array PropertyName)
        , tableMeta: writeImpl (toUnfoldable tableMeta :: Array PropertyName)
        , form: writeImpl (toUnfoldable form :: Array PropertyName)
        , filters: writeImpl (toUnfoldable filters :: Array PropertyName)
        , filtersPresets: writeImpl (toUnfoldable filtersPresets :: Array FiltersPresetsItem)
        , indexes: writeImpl (toUnfoldable indexes :: Array PropertyName)
        , layout: writeImpl layout
        , formLayout: writeImpl (toUnfoldable formLayout :: Array LayoutItem)
        }

data Program
  = Program
    { collections :: List Collection
    }

derive instance genericProgram :: Generic Program _

instance showProgram :: Show Program where
  show = genericShow

instance eqProgram :: Eq Program where
  eq = genericEq

instance WriteForeign Program where
  writeImpl (Program { collections }) =
    writeImpl
      { kind: "Program"
      , collections: writeImpl (toUnfoldable collections :: Array Collection)
      }
