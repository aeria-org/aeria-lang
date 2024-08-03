module Aeria.Semantic.Constraints.RefProperty
  ( sRefAttributes
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
    [ "inline" /\ SingleType AttrBoolean
    , "constraints" /\ SingleType AttrExpr
    , "indexes" /\ SingleType (AttrArrayOf AttrPropertyName)
    , "populate" /\ SingleType (AttrArrayOf AttrPropertyName)
    , "form" /\ SingleType (AttrArrayOf AttrPropertyName)
    ]

sRefAttributes :: CollectionName -> Property -> SemanticM Unit
sRefAttributes collectionName property = validatePropertyAttributes collectionName property attributes
