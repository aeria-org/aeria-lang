module Aeria.Syntax.Tree where

import Data.Maybe (Maybe)
import Prim (Array, Boolean, Number, String)

type Name = String

type MixinPath = String

data Value
  = NumberValue Number
  | StringValue String
  | BooleanValue Boolean
  | Identifier Name

data Condition = Condition Value Oper Value

data Oper
  = LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | Equal

data Typ
  = Typ Name
  | ArrayType Typ
  | ObjectType (Array Property)

data Macro = Macro Name String

data Language = Language
  { mixin :: Maybe MixinPath
  , collection :: CollectionDef
  , router :: Router
  }

data CollectionDef = CollectionDef
  { collectionName :: Name
  , properties     :: PropertiesDef
  , required       :: Maybe RequiredDef
  , getters        :: Maybe Getters
  , table          :: Maybe Table
  }

type RequiredDef = Array RequiredProperty

data RequiredProperty = RequiredProperty Name Attribute

data Attribute = Attribute Name (Array Condition)

type PropertiesDef = Array Property

data Property = Property
  { propertyName        :: Name
  , propertyType        :: Typ
  , propertyAttributes  :: Array Attribute
  }

data Getters = Getters
  { getterName :: Name
  , macro      :: Macro
  }

data Table = Table (Array Name)

data Router = Router Route

data Route = Route
  { httpMethod     :: HttpMethod
  , routePath      :: String
  , routeType      :: Typ
  , documentation  :: Documentation
  }

data HttpMethod
  = POST
  | GET
  | PUT
  | DELETE

data Documentation = Documentation String
