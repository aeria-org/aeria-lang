module Aeria.Driver (compile, compile', compile'') where

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
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)
import Node.FS.Sync (readFile)
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

emitCode :: String -> String -> String -> FilePath -> Output -> Effect Unit
emitCode name js ts outputPath output = do
  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (makeJsFileOutputName name output) code
      Left err -> logShow err
  ) (formatJavascript js)

  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (name <> ".d.ts") code
      Left err -> logShow err
  ) (formatTypescript ts)

compile :: FilePath -> String -> String -> Either Diagnostic (Array (Array String))
compile filepath source output = do
  case parseOutput output of
    Just output' ->
      case compile'' filepath source of
        Right result ->
          pure $
            result
              # map (\(Codegen name js ts) -> [name, ppJavascript output' js, ppTypescript ts])
              # L.toUnfoldable
        Left err -> Left err
    Nothing -> Right []

compile' :: FilePath -> String -> String -> Effect Unit
compile' filepath outputPath output = do
  source <- readSource filepath
  case parseOutput output of
    Just output' ->
      case compile'' filepath source of
        Right program' -> for_ program' (\(Codegen name js ts) ->
          emitCode name (ppJavascript output' js) (ppTypescript ts) outputPath output'
        )
        Left err -> log (ppDiagnostic err)
    Nothing -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"

compile'' :: FilePath -> String -> Either Diagnostic (L.List Codegen)
compile'' filepath source = do
  case runProgram filepath source of
    Right program' ->
      case runSemantic filepath source program' of
        Right _ -> Right $ codegen program'
        Left err -> Left err
    Left err -> Left err

parseOutput :: String -> Maybe Output
parseOutput "commonjs" = Just CommonJs
parseOutput "esnext" = Just EsNext
parseOutput _ = Nothing
