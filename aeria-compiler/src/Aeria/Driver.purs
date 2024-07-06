module Aeria.Driver (compile, checker, compile', compile'') where

import Prelude

import Aeria.Codegen (Codegen(..), codegen)
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (TargetModule(..))
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
import Effect.Console (log)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)
import Node.FS.Sync (readFile)
import Prettier (formatJS, formatTS)

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

makeJsFileName :: String -> TargetModule -> String
makeJsFileName name targetModule = name <> case targetModule of
  CommonJs -> ".js"
  EsNext -> ".mjs"

emitCode :: String -> String -> String -> FilePath -> TargetModule -> Effect Unit
emitCode name jsCode tsCode outputPath targetModule = do
  writeOutput outputPath (makeJsFileName name targetModule) (formatJS jsCode)
  writeOutput outputPath (name <> ".d.ts") (formatTS tsCode)

compile :: FilePath -> String -> String -> Either Diagnostic (Array (Array String))
compile filepath source targetModule = do
  case parseModule targetModule of
    Just targetModule' ->
      case compile'' filepath source of
        Right result ->
          pure $
            result
              # map (\(Codegen name js ts) -> ["collection", name, ppJavascript targetModule' js, ppTypescript ts])
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

parseModule :: String -> Maybe TargetModule
parseModule "commonjs" = Just CommonJs
parseModule "esnext" = Just EsNext
parseModule _ = Nothing
