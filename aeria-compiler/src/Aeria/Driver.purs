module Aeria.Driver where

import Prelude

import Aeria.Codegen (Codegen(..), codegen)
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Codegen.Typescript.Pretty (ppTypescript)
import Aeria.Diagnostic.Message (Diagnostic, ppDiagnostic)
import Aeria.Semantic (runSemantic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List as L
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)
import Node.FS.Sync (readFile)
import Node.Process (exit')
import Prettier.Format (formatJavascript, formatTypescript)

type FilePath = String

writeOutput :: String -> String -> String -> Effect Unit
writeOutput filePath fileName content = do
  Aff.runAff_ (\_ -> pure unit) (mkdir filePath)
  buffer <- fromString content UTF8
  Aff.runAff_ (\_ -> pure unit) (writeFile (filePath <> "/" <> fileName) buffer)

readSource :: String -> Effect String
readSource filePath = do
  content <- readFile filePath
  toString UTF8 content

makeJsFileOutputName :: String -> Output -> String
makeJsFileOutputName name output = name <> case output of
  CommonJs -> ".js"
  EsNext -> ".mjs"

emitCode :: String -> Output -> Codegen -> Effect Unit
emitCode outputPath output (Codegen name js ts)  = do
  let js' = ppJavascript output js
  let ts' = ppTypescript ts
  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (makeJsFileOutputName name output) code
      Left err -> logShow err
  ) (formatJavascript js')

  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (name <> ".d.ts") code
      Left err -> logShow err
  ) (formatTypescript ts')

compile :: FilePath -> String -> Output -> Effect Unit
compile filepath outputPath output = do
  source <- readSource filepath
  case runProgram filepath source of
    Right program' ->
      case runSemantic filepath source program' of
        Right _ -> for_ (codegen program') (emitCode outputPath output)
        Left err -> do
          log (ppDiagnostic err)
          exit' 1
    Left err -> do
      log (ppDiagnostic err)
      exit' 1

compile' :: FilePath -> String -> Either Diagnostic (L.List Codegen)
compile' filepath source = do
  case runProgram filepath source of
    Right program' ->
      case runSemantic filepath source program' of
        Right _ -> Right $ codegen program'
        Left err -> Left err
    Left err -> Left err
