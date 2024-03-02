module Aeria.Syntax.Tree where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

type Ident
  = String

-- Nomes de coleção devem começar com a primeira letra maiuscula
data CollectionName
  = CollectionName Ident

derive instance genericCollectionName :: Generic CollectionName _

instance showCollectionName :: Show CollectionName where
  show = genericShow

instance eqCollectionName :: Eq CollectionName where
  eq = genericEq

instance ordCollectionName :: Ord CollectionName where
  compare = genericCompare

-- Nomes de coleção devem começar com a primeira letra minuscula
data PropertyName
  = PropertyName Ident

derive instance genericPropertyName :: Generic PropertyName _

instance showPropertyName :: Show PropertyName where
  show = genericShow

instance eqPropertyName :: Eq PropertyName where
  eq = genericEq

instance ordPropertyName :: Ord PropertyName where
  compare = genericCompare

data AttributeName
  = AttributeName Ident

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
  = LInteger Int
  | LFloat Number
  | LString String
  | LBoolean Boolean
  | LProperty PropertyName
  | LArray (List Literal)

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

data PropertyType
  = PEnum
  | PFloat
  | PString
  | PInteger
  | PBoolean
  | PRef CollectionName
  | PArray PropertyType
  | PObject Properties

derive instance genericPropertyType :: Generic PropertyType _

instance showPropertyType :: Show PropertyType where
  show x = genericShow x

instance eqPropertyType :: Eq PropertyType where
  eq x = genericEq x

type Required
  = List RequiredProperty

data RequiredProperty
  = RequiredProperty PropertyName (Maybe Expr)

derive instance genericRequiredProperty :: Generic RequiredProperty _

instance showRequiredProperty :: Show RequiredProperty where
  show = genericShow

instance eqRequiredProperty :: Eq RequiredProperty where
  eq = genericEq

type Attributes
  = List Attribute

data Attribute
  = Attribute AttributeName AttributeValue

derive instance genericAttribute :: Generic Attribute _

instance showAttribute :: Show Attribute where
  show = genericShow

instance eqAttribute :: Eq Attribute where
  eq = genericEq

data AttributeValue
  = ALiteral Literal
  | AExpr Expr

derive instance genericAttributeValue :: Generic AttributeValue _

instance showAttributeValue :: Show AttributeValue where
  show = genericShow

instance eqAttributeValue :: Eq AttributeValue where
  eq = genericEq

type Properties
  = List Property

data Property
  = Property
    { name :: PropertyName
    , type_ :: PropertyType
    , attributes :: Attributes
    }

derive instance genericProperty :: Generic Property _

instance showProperty :: Show Property where
  show = genericShow

instance eqProperty :: Eq Property where
  eq = genericEq

data Macro
  = Macro Ident String

derive instance genericMacro :: Generic Macro _

instance showMacro :: Show Macro where
  show = genericShow

instance eqMacro :: Eq Macro where
  eq = genericEq

type Getters
  = List Getter

data Getter
  = Getter
    { name :: PropertyName
    , macro :: Macro
    }

derive instance genericGetters :: Generic Getter _

instance showGetters :: Show Getter where
  show = genericShow

instance eqGetters :: Eq Getter where
  eq = genericEq

type Table
  = List PropertyName

data Collection
  = Collection
    { name :: CollectionName
    , properties :: Properties
    , required :: Required
    , getters :: Getters
    , table :: Table
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
