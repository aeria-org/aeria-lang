module Aeria.Semantic.Constraints.FileProperty
  ( sFileAttributes
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
    [ "accept" /\ SingleType (AttrArrayOf AttrString)
    , "extensions" /\ SingleType (AttrArrayOf AttrString)
    ]

sFileAttributes :: CollectionName -> Property -> SemanticM Unit
sFileAttributes collectionName property = validatePropertyAttributes collectionName property attributes
