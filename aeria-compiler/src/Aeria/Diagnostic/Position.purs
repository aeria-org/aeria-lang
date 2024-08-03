module Aeria.Diagnostic.Position where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data SourcePos = SourcePos Int Int Int -- index, line, column

derive instance genericSourcePos :: Generic SourcePos _

instance showSourcePos :: Show SourcePos where
  show = genericShow

instance eqSourcePos :: Eq SourcePos where
  eq = genericEq

instance ordSourcePos :: Ord SourcePos where
  compare = genericCompare

data Span = Span SourcePos SourcePos

derive instance genericSpan :: Generic Span _

instance showSpan :: Show Span where
  show = genericShow

instance eqSpan :: Eq Span where
  eq = genericEq

instance ordSpan :: Ord Span where
  compare = genericCompare

ghostSpan :: Span
ghostSpan = Span (SourcePos 0 0 0) (SourcePos 0 0 0)
