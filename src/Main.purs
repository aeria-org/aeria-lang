module Main where

import Prelude

import Aeria.Semantic.Monad (runSemantic)
import Aeria.Syntax.Parser (runCollectionP)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)

example1 :: String
example1 = """
  collection Person {
    properties {
      name  str           @max(45)
      email str
    }
  }
"""

main :: Effect Unit
main = do
  let program = runCollectionP (example1)
  case program of
    Right program' ->
      case runSemantic program' of
        Right _ -> log "ok"
        Left err -> logShow err
    Left err -> logShow err
