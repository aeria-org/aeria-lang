module Main where

import Prelude

import Aeria.Codegen.Javascript (codegen)
import Aeria.Semantic.Monad (runSemantic)
import Aeria.Syntax.Parser (runCollectionP)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (logShow)

example :: String
example = """
  collection User {
    properties {
      first_name      str         @minLength(10) @maxLength(45)
      last_name       str
      age             int
      blocked         bool
      responsible     []Person    @indexes([first_name, last_name])
    }
  }
"""

main :: Effect Unit
main = do
  let program = runCollectionP (example)
  case program of
    Right program' ->
      case runSemantic program' of
        Right _ -> logShow (codegen program')
        Left err -> logShow err
    Left err -> logShow err
