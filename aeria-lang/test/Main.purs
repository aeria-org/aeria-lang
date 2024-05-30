module Test.Main where

import Prelude

import Aeria.Diagnostic.Message (ppDiagnostic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Resolver (Program(..), readPrograms)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utils.CompareJSON (compareJSON)
import Yoga.JSON (writeJSON)

syntaxTest name schema golden = do
  it name do
    let program = runProgram name schema
    case program of
      Left err -> fail (ppDiagnostic err)
      Right program' -> do
        let json = writeJSON program'
        compareJSON json golden `shouldEqual` true

-- codegenTest name schema golden = do
--   it name do
--     let program = compile name schema
--     case program of
--       Left err -> fail (ppDiagnostic err)
--       Right program' -> do

        -- let json = writeJSON program'
        -- compareJSON json golden `shouldEqual` true

main :: Effect Unit
main = do
  programs <- readPrograms "./test/Suite/Syntax"
  launchAff_ $ runSpec [consoleReporter] do
    describe "Syntax" do
      for_ programs (\(Program { name, schema, golden }) ->
        syntaxTest name schema golden)
