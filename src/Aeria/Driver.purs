module Aeria.Driver where

import Prelude
import Aeria.Codegen.Javascript (codegen)
import Aeria.Semantic (runSemantic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)

compile :: String -> Effect Unit
compile source = do
  let
    program = runProgram source
  case program of
    Right program' -> case runSemantic program' of
      Right _ -> logShow (codegen program')
      Left err -> logShow err
    Left err -> logShow err
