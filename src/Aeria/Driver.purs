module Aeria.Driver where

import Prelude

import Aeria.Codegen (Codegen(..), codegen)
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Codegen.Typescript.Pretty (ppTypescript)
import Aeria.Diagnostic.Message (ppDiagnostic)
import Aeria.Semantic (runSemantic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)
import Node.FS.Sync (readFile)
import Node.Process (exit')

writeOutput :: String -> String -> String -> Effect Unit
writeOutput filePath fileName content = do
  Aff.runAff_ (\_ -> pure unit) (mkdir filePath)
  buffer <- fromString content UTF8
  Aff.runAff_ (\_ -> pure unit) (writeFile (filePath <> "/" <> fileName) buffer)

readSource :: String -> Effect String
readSource filePath = do
  content <- readFile filePath
  toString UTF8 content

makeExtenssion :: Output -> String
makeExtenssion = case _ of
  CommonJs -> ".js"
  EsNext -> ".mjs"

compile :: String -> String -> Output -> Effect Unit
compile filepath outputPath output = do
  source <- readSource filepath
  let program = runProgram filepath source
  case program of
    Right program' ->
      case runSemantic filepath source program' of
        Right _ -> do
          for_ (codegen program')
            ( \(Codegen name jsFile tsFile) -> do
                writeOutput outputPath (name <> makeExtenssion output) (ppJavascript output jsFile)
                writeOutput outputPath (name <> ".d.ts") (ppTypescript tsFile)
            )
        Left err -> do
          log (ppDiagnostic err)
          exit' 1
    Left err -> do
      log (ppDiagnostic err)
      exit' 1
