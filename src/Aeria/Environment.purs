module Aeria.Environment where

import Data.Map.Internal (Map, insert, lookup, empty) as M
import Data.Maybe (Maybe)

data Environment a = Environment (M.Map String a)

empty :: forall a. Environment a
empty = Environment M.empty

lookup :: forall a. String -> Environment a -> Maybe a
lookup name (Environment env) = M.lookup name env

extend :: forall a. String -> a -> Environment a -> Environment a
extend name ty (Environment env) = Environment (M.insert name ty env)
