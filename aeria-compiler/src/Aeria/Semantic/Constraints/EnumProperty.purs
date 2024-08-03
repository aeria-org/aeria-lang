module Aeria.Semantic.Constraints.EnumProperty
  ( sEnumAttributes
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
    [ "values" /\ SingleType (AttrArrayOf AttrString)
    , "default" /\ SingleType AttrString
    ]

sEnumAttributes :: CollectionName -> Property -> SemanticM Unit
sEnumAttributes collectionName property = validatePropertyAttributes collectionName property attributes
