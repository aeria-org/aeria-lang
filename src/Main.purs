module Main where

import Prelude

import Aeria.Syntax.Parser (runP)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)

example1 :: String
example1 = """
  collection Person {
    properties {
      name  str
      email str
      posts []{
        title       str
        description str
        body        str
      }
    }
  }
"""

main :: Effect Unit
main = do
  let tree = runP (example1)
  case tree of
    Right t -> log $ show t
    Left err -> log $ show err

