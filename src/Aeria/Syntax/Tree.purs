module Aeria.Syntax.Tree where

import Prelude
import Data.List (List)
import Data.Maybe (Maybe)

data Name = Name String
instance showName :: Show Name where
  show (Name n) = "(Name " <> show n <> ")"

data CollectionName = CollectionName String
instance showCollectionName :: Show CollectionName where
  show (CollectionName cn) = "(CollectionName " <> show cn <> ")"

data PropertyName = PropertyName String
instance showPropertyName :: Show PropertyName where
  show (PropertyName pn) = "(PropertyName " <> show pn <> ")"

type MixinPath = String

data Value
  = VNumber Number
  | VString String
  | VBoolean Boolean
  | VVar Name
instance showValue :: Show Value where
  show (VNumber n) = "(VNumber " <> show n <> ")"
  show (VString s) = "(VString " <> show s <> ")"
  show (VBoolean b) = "(VBoolean " <> show b <> ")"
  show (VVar n) = "(VVar " <> show n <> ")"

data Condition = Condition Value Oper Value
instance showCondition :: Show Condition where
  show (Condition v1 o v2) = "(Condition " <> show v1 <> " " <> show o <> " " <> show v2 <> ")"

data Oper
  = Lt
  | Gt
  | Lte
  | Gte
  | Eq
instance showOper :: Show Oper where
  show Lt = "Lt"
  show Gt = "Gt"
  show Lte = "Lte"
  show Gte = "Gte"
  show Eq = "Eq"

data Typ
  = TName Name
  | TArray Typ
  | TObject (List Property)
instance showTyp :: Show Typ where
  show (TName n) = "(TName " <> show n <> ")"
  show (TArray t) = "(TArray " <> show t <> ")"
  show (TObject lp) = "(TObject " <> show lp <> ")"

data Macro = Macro Name String
instance showMacro :: Show Macro where
  show (Macro n s) = "(Macro " <> show n <> " " <> show s <> ")"

data Program = Program
  { -- mixin :: Maybe MixinPath
   collection :: Collection
  -- , router :: Router
  }
instance showProgram :: Show Program where
  show (Program {collection}) = "(Program {collection = " <> show collection <> "})"

data Collection = Collection
  { collectionName :: CollectionName
  , properties     :: Properties
  , required       :: Maybe Required
  , getters        :: Maybe Getters
  , table          :: Maybe Table
  }
instance showCollection :: Show Collection where
  show (Collection {collectionName, properties, required, getters, table}) =
    "(Collection {collectionName = " <> show collectionName <>
    ", properties = " <> show properties <>
    ", required = " <> show required <>
    ", getters = " <> show getters <>
    ", table = " <> show table <> "})"

type Required = Array RequiredProperty

data RequiredProperty = RequiredProperty Name Attribute
instance showRequiredProperty :: Show RequiredProperty where
  show (RequiredProperty n a) = "(RequiredProperty " <> show n <> " " <> show a <> ")"

data Attribute = Attribute Name (Array Condition)
instance showAttribute :: Show Attribute where
  show (Attribute n ac) = "(Attribute " <> show n <> " " <> show ac <> ")"

type Properties = List Property

data Property = Property
  { propertyName        :: PropertyName
  , propertyType        :: Typ
  , propertyAttributes  :: Array Attribute
  }
instance showProperty :: Show Property where
  show (Property {propertyName, propertyType, propertyAttributes}) =
    "(Property {propertyName = " <> show propertyName <>
    ", propertyType = " <> show propertyType <>
    ", propertyAttributes = " <> show propertyAttributes <> "})"

data Getters = Getters
  { getterName :: Name
  , macro      :: Macro
  }
instance showGetters :: Show Getters where
  show (Getters {getterName, macro}) = "(Getters {getterName = " <> show getterName <> ", macro = " <> show macro <> "})"

data Table = Table (Array Name)
instance showTable :: Show Table where
  show (Table an) = "(Table " <> show an <> ")"

data Router = Router
  { httpMethod     :: HttpMethod
  , routePath      :: String
  , routeType      :: Typ
  , documentation  :: Doc
  }

instance showRoute :: Show Router where
  show (Router {httpMethod, routePath, routeType, documentation}) =
    "(Router {httpMethod = " <> show httpMethod <>
    ", routePath = " <> show routePath <>
    ", routeType = " <> show routeType <>
    ", documentation = " <> show documentation <> "})"

data HttpMethod
  = POST
  | GET
  | PUT
  | DELETE
instance showHttpMethod :: Show HttpMethod where
  show POST = "POST"
  show GET = "GET"
  show PUT = "PUT"
  show DELETE = "DELETE"

data Doc = Doc String
instance showDoc :: Show Doc where
  show (Doc s) = "(Doc " <> show s <> ")"
