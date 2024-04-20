module Aeria.Syntax.Tree where

import Prelude

import Aeria.Diagnostic.Position (Span)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

type Ident
  = String

data CollectionName
  = CollectionName Span Ident

derive instance genericCollectionName :: Generic CollectionName _

instance showCollectionName :: Show CollectionName where
  show = genericShow

instance eqCollectionName :: Eq CollectionName where
  eq = genericEq

instance ordCollectionName :: Ord CollectionName where
  compare = genericCompare

data PropertyName
  = PropertyName Span Ident

derive instance genericPropertyName :: Generic PropertyName _

instance showPropertyName :: Show PropertyName where
  show = genericShow

instance eqPropertyName :: Eq PropertyName where
  eq = genericEq

instance ordPropertyName :: Ord PropertyName where
  compare = genericCompare

data AttributeName
  = AttributeName Span Ident

derive instance genericAttributeName :: Generic AttributeName _

instance showAttributeName :: Show AttributeName where
  show = genericShow

instance eqAttributeName :: Eq AttributeName where
  eq = genericEq

data Typ
  = TInteger
  | TFloat
  | TString
  | TBoolean
  | TProperty
  | TArray

derive instance genericTyp :: Generic Typ _

instance showTyp :: Show Typ where
  show = genericShow

instance eqTyp :: Eq Typ where
  eq = genericEq

data Literal
  = LInteger Span Int
  | LFloat Span Number
  | LString Span String
  | LBoolean Span Boolean
  | LProperty Span PropertyName
  | LArray Span (List Literal)

derive instance genericLiteral :: Generic Literal _

instance showLiteral :: Show Literal where
  show x = genericShow x

instance eqLiteral :: Eq Literal where
  eq x = genericEq x

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
  | EExists Expr
  | ENot Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance eqExpr :: Eq Expr where
  eq x = genericEq x

data Cond = Cond Span Expr

derive instance genericCond :: Generic Cond _

instance showCond :: Show Cond where
  show x = genericShow x

instance eqCond :: Eq Cond where
  eq x = genericEq x

data PropertyType
  = PEnum Span
  | PFloat Span
  | PString Span
  | PInteger Span
  | PBoolean Span
  | PRef Span CollectionName
  | PArray Span PropertyType
  | PObject Span CollectionProperties

derive instance genericPropertyType :: Generic PropertyType _

instance showPropertyType :: Show PropertyType where
  show x = genericShow x

instance eqPropertyType :: Eq PropertyType where
  eq x = genericEq x

type CollectionRequired
  = List Required

data Required
  = Required Span PropertyName (Maybe Cond)

derive instance genericRequired :: Generic Required _

instance showRequired :: Show Required where
  show = genericShow

instance eqRequired :: Eq Required where
  eq = genericEq

type Attributes
  = List Attribute

data Attribute
  = Attribute Span AttributeName AttributeValue

derive instance genericAttribute :: Generic Attribute _

instance showAttribute :: Show Attribute where
  show = genericShow

instance eqAttribute :: Eq Attribute where
  eq = genericEq

data AttributeValue
  = ALiteral Span Literal
  | AExpr Span Expr

derive instance genericAttributeValue :: Generic AttributeValue _

instance showAttributeValue :: Show AttributeValue where
  show = genericShow

instance eqAttributeValue :: Eq AttributeValue where
  eq = genericEq

type CollectionProperties
  = List Property

data Property
  = Property
    { span :: Span
    , name :: PropertyName
    , type_ :: PropertyType
    , attributes :: Attributes
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

type CollectionGetters
  = List Getter

data Getter
  = Getter
    { span :: Span
    , name :: PropertyName
    , macro :: Macro
    }

derive instance genericGetter :: Generic Getter _

instance showGetter :: Show Getter where
  show = genericShow

instance eqGetter :: Eq Getter where
  eq = genericEq

type CollectionTable
  = List TableItem

data TableItem = TableItem Span PropertyName

derive instance genericTableItem :: Generic TableItem _

instance showTableItem :: Show TableItem where
  show = genericShow

instance eqTableItem :: Eq TableItem where
  eq = genericEq

type CollectionTableMeta
  = List TableMetaItem

data TableMetaItem = TableMetaItem Span PropertyName

derive instance genericTableMetaItem :: Generic TableMetaItem _

instance showTableMetaItem :: Show TableMetaItem where
  show = genericShow

instance eqTableMetaItem :: Eq TableMetaItem where
  eq = genericEq


type CollectionForm
  = List FormItem

data FormItem = FormItem Span PropertyName

derive instance genericFormItem :: Generic FormItem _

instance showFormItem :: Show FormItem where
  show = genericShow

instance eqFormItem :: Eq FormItem where
  eq = genericEq

type CollectionFilters
  = List FilterItem

data FilterItem = FilterItem Span PropertyName

derive instance genericFilterItem :: Generic FilterItem _

instance showFilterItem :: Show FilterItem where
  show = genericShow

instance eqFilterItem :: Eq FilterItem where
  eq = genericEq

type CollectionIndexes
  = List IndexesItem

data IndexesItem = IndexesItem Span PropertyName

derive instance genericIndexesItem :: Generic IndexesItem _

instance showIndexesItem :: Show IndexesItem where
  show = genericShow

instance eqIndexesItem :: Eq IndexesItem where
  eq = genericEq


data CollectionSearch = CollectionSearch
  { placeholder :: Maybe String,
    indexes :: List PropertyName
  }

derive instance genericCollectionSearch :: Generic CollectionSearch _

instance showCollectionSearch :: Show CollectionSearch where
  show = genericShow

instance eqCollectionSearch :: Eq CollectionSearch where
  eq = genericEq

data CollectionIcon = CollectionIcon String

derive instance genericCollectionIcon :: Generic CollectionIcon _

instance showCollectionIcon :: Show CollectionIcon where
  show = genericShow

instance eqCollectionIcon :: Eq CollectionIcon where
  eq = genericEq

data CollectionOwned = CollectionOwned Boolean

derive instance genericCollectionOwned :: Generic CollectionOwned _

instance showCollectionOwned :: Show CollectionOwned where
  show = genericShow

instance eqCollectionOwned :: Eq CollectionOwned where
  eq = genericEq

data CollectionTimestamps = CollectionTimestamps Boolean

derive instance genericCollectionTimestamps :: Generic CollectionTimestamps _

instance showCollectionTimestamps :: Show CollectionTimestamps where
  show = genericShow

instance eqCollectionTimestamps :: Eq CollectionTimestamps where
  eq = genericEq

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

type CollectionLayout = List LayoutItem

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

data LayoutItemCondition = LayoutItemCondition PropertyName Cond

derive instance genericLayoutItemCondition :: Generic LayoutItemCondition _

instance showLayoutItemCondition :: Show LayoutItemCondition where
  show = genericShow

instance eqLayoutItemCondition :: Eq LayoutItemCondition where
  eq = genericEq

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

data CollectionImmutable
  = CollectionImmutableBool Boolean
  | CollectionImmutableList (List ImmutableItem)

derive instance genericCollectionImmutable :: Generic CollectionImmutable _

instance showCollectionImmutable :: Show CollectionImmutable where
  show = genericShow

instance eqCollectionImmutable :: Eq CollectionImmutable where
  eq = genericEq

data ImmutableItem = ImmutableItem Span PropertyName

derive instance genericImmutableItem :: Generic ImmutableItem _

instance showImmutableItem :: Show ImmutableItem where
  show = genericShow

instance eqImmutableItem :: Eq ImmutableItem where
  eq = genericEq

type CollectionWritable = List WritableItem

data WritableItem = WritableItem Span PropertyName

derive instance genericWritableItem :: Generic WritableItem _

instance showWritableItem :: Show WritableItem where
  show = genericShow

instance eqWritableItem :: Eq WritableItem where
  eq = genericEq

type CollectionFunctions = List FunctionItem

data FunctionItem = FunctionItem Span PropertyName

derive instance genericFunctionItem :: Generic FunctionItem _

instance showFunctionItem :: Show FunctionItem where
  show = genericShow

instance eqFunctionItem :: Eq FunctionItem where
  eq = genericEq

type CollectionSecurity = List SecurityItem

data SecurityItem = SecurityItem
  { span :: Span
  , functionName :: PropertyName
  , rateLimiting :: Maybe SecurityRateLimiting
  , logging :: Maybe SecurityLogging
  }

derive instance genericSecurityItem :: Generic SecurityItem _

instance showSecurityItem :: Show SecurityItem where
  show = genericShow

instance eqSecurityItem :: Eq SecurityItem where
  eq = genericEq

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

data SecurityLogging = SecurityLogging
  { span :: Span
  , strategy :: Maybe String
  }

derive instance genericSecurityLogging :: Generic SecurityLogging _

instance showSecurityLogging :: Show SecurityLogging where
  show = genericShow

instance eqSecurityLogging :: Eq SecurityLogging where
  eq = genericEq

type CollectionPresets = List PresetItem

data PresetItem = PresetItem Span PropertyName

derive instance genericPresetItem :: Generic PresetItem _

instance showPresetItem :: Show PresetItem where
  show = genericShow

instance eqPresetItem :: Eq PresetItem where
  eq = genericEq

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

data Collection
  = Collection
    { span :: Span
    , name :: CollectionName
    , icon :: Maybe CollectionIcon
    , owned :: Maybe CollectionOwned
    , timestamps :: Maybe CollectionTimestamps
    , immutable :: Maybe CollectionImmutable
    , temporary :: Maybe CollectionTemporary
    , presets :: CollectionPresets
    , writable :: CollectionWritable
    , functions :: CollectionFunctions
    , security :: CollectionSecurity
    , properties :: CollectionProperties
    , required :: CollectionRequired
    , getters :: CollectionGetters
    , table :: CollectionTable
    , tableMeta :: CollectionTableMeta
    , form :: CollectionForm
    , filters :: CollectionFilters
    , filtersPresets :: CollectionFiltersPresets
    , indexes :: CollectionIndexes
    , layout :: CollectionLayout
    , search :: Maybe CollectionSearch
    }

derive instance genericCollection :: Generic Collection _

instance showCollection :: Show Collection where
  show = genericShow

instance eqCollection :: Eq Collection where
  eq = genericEq

data Program
  = Program
    { collections :: List Collection
    }

derive instance genericProgram :: Generic Program _

instance showProgram :: Show Program where
  show = genericShow

instance eqProgram :: Eq Program where
  eq = genericEq
