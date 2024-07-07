module Data.String.Utils where

import Prelude

import Data.Array (intercalate)
import Data.CodePoint.Unicode (toLower)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), fromCodePointArray, split, uncons)

ucLower :: String -> String
ucLower "" = ""
ucLower str =
  case uncons str of
    Just { head, tail } -> fromCodePointArray (toLower head) <> tail
    Nothing -> ""

concatWith :: forall a. Array a  ->  (a -> String)-> String
concatWith xs f  =
  xs
    # map f
    # intercalate ","

splitLines :: String -> Array String
splitLines str = split (Pattern "\n") str
