module Aeria.Syntax.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Name = Name String

derive instance genericName :: Generic Name _

instance showName :: Show Name where
  show = genericShow

instance eqName :: Eq Name where
  eq = genericEq

data CollectionName = CollectionName String

derive instance genericCollectionName :: Generic CollectionName _

instance showCollectionName :: Show CollectionName where
  show = genericShow

instance eqCollectionName :: Eq CollectionName where
  eq = genericEq

data PropertyName = PropertyName String

derive instance genericPropertyName :: Generic PropertyName _

instance showPropertyName :: Show PropertyName where
  show = genericShow

instance eqPropertyName :: Eq PropertyName where
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

data Value
  = VInteger Int
  | VFloat Number
  | VString String
  | VBoolean Boolean
  | VProperty Name
  | VArray (List Value)

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show x = genericShow x

instance eqValue :: Eq Value where
  eq x = genericEq x

data Expr
  = EValue Value
  | EIn Expr Expr
  | EExists Expr
  | ENot Expr
  | ELt Expr Expr
  | EGt Expr Expr
  | ELte Expr Expr
  | EGte Expr Expr
  | EEq Expr Expr
  | EOr Expr Expr
  | EAnd Expr Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

instance eqExpr :: Eq Expr where
  eq x = genericEq x

data PropertyType
  = PEnum
  | PFloat
  | PString
  | PInteger
  | PFile
  | PBoolean
  | PArray PropertyType
  | PObject Properties
  | PCollection CollectionName

derive instance genericPropertyType :: Generic PropertyType _

instance showPropertyType :: Show PropertyType where
  show x = genericShow x

instance eqPropertyType :: Eq PropertyType where
  eq x = genericEq x

data Macro = Macro Name String

derive instance genericMacro :: Generic Macro _

instance showMacro :: Show Macro where
  show = genericShow

instance eqMacro :: Eq Macro where
  eq = genericEq

data Program = Program
  { collection :: Collection
  }

derive instance genericProgram :: Generic Program _

instance showProgram :: Show Program where
  show = genericShow

instance eqProgram :: Eq Program where
  eq = genericEq

data Collection = Collection
  { collectionName            :: CollectionName
  , collectionProperties      :: Properties
  , collectionRequired        :: Maybe Required
  , collectionGetters         :: Maybe Getters
  , collectionTable           :: Maybe Table
  }

derive instance genericCollection :: Generic Collection _

instance showCollection :: Show Collection where
  show = genericShow

instance eqCollection :: Eq Collection where
  eq = genericEq

type Required = List RequiredProperty

data RequiredProperty = RequiredProperty PropertyName (Maybe Expr)

derive instance genericRequiredProperty :: Generic RequiredProperty _

instance showRequiredProperty :: Show RequiredProperty where
  show = genericShow

instance eqRequiredProperty :: Eq RequiredProperty where
  eq = genericEq

type Attributes = List Attribute

data Attribute = Attribute Name Value

derive instance genericAttribute :: Generic Attribute _

instance showAttribute :: Show Attribute where
  show = genericShow

instance eqAttribute :: Eq Attribute where
  eq = genericEq

type Properties = List Property

data Property = Property
  { propertyName        :: PropertyName
  , propertyType        :: PropertyType
  , propertyAttributes  :: Attributes
  }

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

instance eqProperty :: Eq Property where
  eq = genericEq

type Getters = List Getter

data Getter = Getter
  { getterName      :: PropertyName
  , getterMacro     :: Macro
  }

derive instance genericGetters :: Generic Getter _

instance showGetters :: Show Getter where
  show = genericShow

instance eqGetters :: Eq Getter where
  eq = genericEq

type Table = List PropertyName
