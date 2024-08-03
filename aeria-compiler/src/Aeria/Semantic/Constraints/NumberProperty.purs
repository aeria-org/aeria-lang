module Aeria.Semantic.Constraints.NumberProperty
  ( sNumberAttributes
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
    [ "minimum" /\ MultipleTypes [AttrNum, AttrInteger]
    , "maximum" /\ MultipleTypes [AttrNum, AttrInteger]
    , "exclusiveMinimum" /\ MultipleTypes [AttrNum, AttrInteger]
    , "exclusiveMaximum" /\ MultipleTypes [AttrNum, AttrInteger]
    , "default" /\ MultipleTypes [AttrInteger, AttrNum]
    , "placeholder" /\ MultipleTypes [AttrString]
    ]

sNumberAttributes :: CollectionName -> Property -> SemanticM Unit
sNumberAttributes collectionName property = validatePropertyAttributes collectionName property attributes
