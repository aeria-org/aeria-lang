module Data.String.Utils where

import Prelude

import Data.Array (intercalate)
import Data.CodePoint.Unicode (toUpper)
import Data.Maybe (Maybe(..))
import Data.String (fromCodePointArray, uncons)
import Data.String.CodeUnits (fromCharArray)

ucfirst :: String -> String
ucfirst "" = ""
ucfirst str =
  case uncons str of
    Just { head, tail } -> fromCodePointArray (toUpper head) <> tail
    Nothing -> ""

concatChar :: Char -> String -> String
concatChar char str = fromCharArray [char] <> str

concatWith :: forall a. Array a  ->  (a -> String)-> String
concatWith xs f  =
  xs
    # map f
    # intercalate ","
