module Aeria.Semantic.Constraints.ArrayProperty
  ( sArrayAttributes
  , arrayAttributeNames
  )
  where

import Aeria.Semantic.Attributes (AttributeConstraint(..), AttributeConstraints, AttributeType(..), validatePropertyAttributes)
import Aeria.Semantic.Internal (SemanticM)
import Aeria.Syntax.Tree

import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Tuple.Nested ((/\))
import Prelude (Unit)

attributes ∷ AttributeConstraints
attributes =
  M.fromFoldable
    [ "default" /\ SingleType AttrArray
    , "minItems" /\ SingleType AttrInteger
    , "maxItems" /\ SingleType AttrInteger
    , "uniqueItems" /\ SingleType AttrBoolean
    ]

arrayAttributeNames ∷ Array String
arrayAttributeNames =
  [ "default"
  , "minItems"
  , "maxItems"
  , "uniqueItems"
  ]

getArrayAttributes ∷ L.List Attribute -> L.List Attribute
getArrayAttributes = L.filter (\(Attribute _ attributeName _) -> getName attributeName `A.elem` arrayAttributeNames)

sArrayAttributes :: CollectionName -> Property -> SemanticM Unit
sArrayAttributes collectionName (Property { span, name, type_, attributes: propertyAttributes }) =
  validatePropertyAttributes collectionName arrayProperty attributes
  where
    arrayProperty = Property { span, name, type_, attributes: arrayAttributes }
    arrayAttributes = getArrayAttributes propertyAttributes
