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

data Value
  = VInt Int
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

data Condition = Condition Value Oper Value

derive instance genericCondition :: Generic Condition _

instance showCondition :: Show Condition where
  show = genericShow

instance eqCondition :: Eq Condition where
  eq = genericEq

data Oper
  = Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Or
  | And

derive instance genericOper :: Generic Oper _

instance showOper :: Show Oper where
  show = genericShow

instance eqOper :: Eq Oper where
  eq = genericEq

data Typ
  = TEnum
  | TFloat
  | TString
  | TInteger
  | TBoolean
  | TArray Typ
  | TObject (List Property)
  | TCollection CollectionName

derive instance genericTyp :: Generic Typ _

instance showTyp :: Show Typ where
  show x = genericShow x

instance eqTyp :: Eq Typ where
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

data RequiredProperty = RequiredProperty PropertyName (Maybe Condition)

derive instance genericRequiredProperty :: Generic RequiredProperty _

instance showRequiredProperty :: Show RequiredProperty where
  show = genericShow

instance eqRequiredProperty :: Eq RequiredProperty where
  eq = genericEq

data Attribute = Attribute Name Value

derive instance genericAttribute :: Generic Attribute _

instance showAttribute :: Show Attribute where
  show = genericShow

instance eqAttribute :: Eq Attribute where
  eq = genericEq

type Properties = List Property

data Property = Property
  { propertyName        :: PropertyName
  , propertyType        :: Typ
  , propertyAttributes  :: List Attribute
  }

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

instance eqProperty :: Eq Property where
  eq = genericEq

type Getters = List Getter

data Getter = Getter
  { getterName :: PropertyName
  , getterMacro      :: Macro
  }

derive instance genericGetters :: Generic Getter _

instance showGetters :: Show Getter where
  show = genericShow

instance eqGetters :: Eq Getter where
  eq = genericEq

data Table = Table (List PropertyName)

derive instance genericTable :: Generic Table _

instance showTable :: Show Table where
  show = genericShow

instance eqTable :: Eq Table where
  eq = genericEq
