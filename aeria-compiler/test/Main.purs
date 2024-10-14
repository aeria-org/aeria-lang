module Test.Main where

import Prelude

import Aeria.Codegen (Codegen(..))
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (TargetModule(..))
import Aeria.Codegen.Typescript.Pretty (ppTypescript)
import Aeria.Diagnostic.Message (ppDiagnostic)
import Aeria.Driver (compile'')
import Aeria.Syntax.Parser (runParserProgram)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Prettier (formatJS, formatTS)
import Test.Resolver (Program(..), readPrograms)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Utils.CompareJSON (compareJSON)
import Yoga.JSON (writeJSON)

syntaxTest ∷ ∀ (m4 ∷ Type -> Type) (g7 ∷ Type -> Type). Monad m4 ⇒ MonadThrow Error g7 ⇒ String -> String -> String -> SpecT g7 Unit m4 Unit
syntaxTest testName schema golden = do
  it testName do
    let program = runParserProgram testName schema
    case program of
      Left err -> fail (ppDiagnostic err)
      Right program' -> do
        let json = writeJSON program'
        compareJSON json golden `shouldEqual` true

codegenTest :: forall m52. Monad m52 => TargetModule -> String -> String -> String -> SpecT Aff Unit m52 Unit
codegenTest targetModule testName schema golden = do
  it testName do
    let codegen = compile'' targetModule "<stdin>" schema
    case codegen of
      Left err -> fail (ppDiagnostic err)
      Right codegen' -> do
        for_ codegen' (\(Codegen _ jsStatments _) -> do
          let code = ppJavascript targetModule jsStatments
          formatJS code `shouldEqual` formatJS golden)

typegenTest ∷ ∀ (m23 ∷ Type -> Type). Monad m23 ⇒ String -> String -> String -> SpecT Aff Unit m23 Unit
typegenTest testName schema golden = do
  it testName do
    let codegen = compile'' EsNext "<stdin>" schema
    case codegen of
      Left err -> fail (ppDiagnostic err)
      Right codegen' -> do
        for_ codegen' (\(Codegen _ _ ts) -> do
          let code = ppTypescript ts
          formatTS code `shouldEqual` formatTS golden)

main :: Effect Unit
main = do
  syntax <- readPrograms "./test/Suite/Syntax" ".golden" false
  commonJs <- readPrograms "./test/Suite/Codegen" ".js" true
  esnext <- readPrograms "./test/Suite/Codegen" ".mjs" true
  typescript <- readPrograms "./test/Suite/Codegen" ".d.ts" true

  launchAff_ $ runSpec [consoleReporter] do
    describe "Syntax" do
      for_ syntax (\(Program { name, schema, golden }) ->
        syntaxTest name schema golden)

    describe "Codegen" do
      describe "CommonJs" do
        for_ commonJs (\(Program { name, schema, golden }) ->
          codegenTest CommonJs name schema golden)

      describe "EsNext" do
        for_ esnext (\(Program { name, schema, golden }) ->
          codegenTest EsNext name schema golden)

      describe "Typescript" do
        for_ typescript (\(Program { name, schema, golden }) ->
          typegenTest name schema golden)
