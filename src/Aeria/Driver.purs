module Aeria.Driver where

import Prelude

import Aeria.Codegen.Javascript (Codegen(..), codegen)
import Aeria.Codegen.Javascript.Pretty (ppJavascript)
import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Codegen.Typescript.Pretty (ppTypescript)
import Aeria.Semantic (runSemantic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Node.Buffer (fromString, toString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)
import Node.FS.Sync (readFile)

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
compile sourcePath outputPath output = do
  source <- readSource sourcePath
  let program = runProgram source
  case program of
    Right program' -> case runSemantic program' of
      Right _ -> do
        for_ (codegen program')
          ( \(Codegen name jsFile tsFile) -> do
              writeOutput outputPath (name <> makeExtenssion output) (ppJavascript output jsFile)
              writeOutput outputPath (name <> ".d.ts") (ppTypescript tsFile)
          )
      Left err -> logShow err
    Left err -> logShow err
