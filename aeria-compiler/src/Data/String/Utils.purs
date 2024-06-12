module Data.String.Utils where

import Prelude

import Data.Array (intercalate)
import Data.CodePoint.Unicode (toUpper)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), fromCodePointArray, split, uncons)

ucfirst :: String -> String
ucfirst "" = ""
ucfirst str =
  case uncons str of
    Just { head, tail } -> fromCodePointArray (toUpper head) <> tail
    Nothing -> ""

concatWith :: forall a. Array a  ->  (a -> String)-> String
concatWith xs f  =
  xs
    # map f
    # intercalate ","

splitLines :: String -> Array String
splitLines str = split (Pattern "\n") str
