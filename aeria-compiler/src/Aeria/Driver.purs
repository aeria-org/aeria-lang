module Aeria.Driver (compile, checker, compile', compile'') where

import Prelude

import Aeria.Codegen (Codegen(..), codegen)
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (Module(..))
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

makeJsFileName :: String -> Module -> String
makeJsFileName name module_ = name <> case module_ of
  CommonJs -> ".js"
  EsNext -> ".mjs"

emitCode :: String -> String -> String -> FilePath -> Module -> Effect Unit
emitCode name js ts outputPath module_ = do
  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (makeJsFileName name module_) code
      Left err -> logShow err
  ) (formatJavascript js)

  Aff.runAff_ (
    case _ of
      Right code -> writeOutput outputPath (name <> ".d.ts") code
      Left err -> logShow err
  ) (formatTypescript ts)

compile :: FilePath -> String -> String -> Either Diagnostic (Array (Array String))
compile filepath source module_ = do
  case parseModule module_ of
    Just module_' ->
      case compile'' filepath source of
        Right result ->
          pure $
            result
              # map (\(Codegen name js ts) -> ["collection", name, ppJavascript module_' js, ppTypescript ts])
              # L.toUnfoldable
        Left err -> Left err
    Nothing -> Right []

checker :: FilePath -> String -> Either Diagnostic Unit
checker filepath source = do
  case runProgram filepath source of
    Right program' -> runSemantic filepath source program'
    Left err -> Left err

compile' :: FilePath -> String -> String -> Effect Unit
compile' filepath outputPath output = do
  source <- readSource filepath
  case parseModule output of
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

parseModule :: String -> Maybe Module
parseModule "commonjs" = Just CommonJs
parseModule "esnext" = Just EsNext
parseModule _ = Nothing
