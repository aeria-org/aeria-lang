module Aeria.Semantic.Constraints.BooleanProperty
  ( sBooleanAttributes
  )
  where

import Aeria.Semantic.Attributes (AttributeConstraint(..), AttributeConstraints, AttributeType(..), validatePropertyAttributes)
import Aeria.Semantic.Internal (SemanticM)
import Aeria.Syntax.Tree

import Data.Map as M
import Data.Tuple.Nested ((/\))
import Prelude (Unit)

attributes ∷ AttributeConstraints
attributes =
  M.fromFoldable
    [ "default" /\ SingleType AttrBoolean
    ]

sBooleanAttributes :: CollectionName -> Property -> SemanticM Unit
sBooleanAttributes collectionName propeprty = validatePropertyAttributes collectionName propeprty attributes
