module Aeria.Semantic.Constraints.StringProperty
  ( sStringAttributes
  )
  where

import Aeria.Semantic.Attributes (AttributeConstraint(..), AttributeConstraints, AttributeType(..), validatePropertyAttributes)
import Aeria.Semantic.Internal (SemanticM)
import Aeria.Syntax.Tree

import Data.Map as M
import Data.Tuple.Nested ((/\))
import Prelude (Unit)

inputType :: Array String
inputType = [ "text", "email", "password", "search", "time", "month" ]

format :: Array String
format = [ "date", "date-time" ]

attributes ∷ AttributeConstraints
attributes =
  M.fromFoldable
    [ "minLength" /\ SingleType AttrInteger
    , "maxLength" /\ SingleType AttrInteger
    , "default" /\ SingleType AttrString
    , "placeholder" /\ SingleType AttrString
    , "element" /\ SingleType (AttrEnum ["input", "textarea"])
    , "mask" /\ MultipleTypes [AttrString, AttrArrayOf AttrString]
    , "format" /\ SingleType (AttrEnum format)
    , "inputType" /\ SingleType (AttrEnum inputType)
    ]

sStringAttributes :: CollectionName -> Property -> SemanticM Unit
sStringAttributes collectionName property = validatePropertyAttributes collectionName property attributes
