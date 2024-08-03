module Aeria.Semantic.Constraints.ConstProperty
  ( sConstAttributes
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
    [ "value" /\ MultipleTypes [AttrBoolean, AttrNum, AttrInteger, AttrString, AttrUndefined, AttrNull]
    ]

sConstAttributes :: CollectionName -> Property -> SemanticM Unit
sConstAttributes collectionName property = validatePropertyAttributes collectionName property attributes
