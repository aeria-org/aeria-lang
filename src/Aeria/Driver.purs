module Aeria.Driver where

import Prelude
import Aeria.Codegen.Javascript (codegen)
import Aeria.Codegen.Javascript.Tree (Output(..), ppJsTree)
import Aeria.Semantic (runSemantic)
import Aeria.Syntax.Parser (runProgram)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class.Console (logShow)
import Node.Buffer (fromString)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdir, writeFile)

writeOutput :: String -> String -> String -> Effect Unit
writeOutput filePath fileName content = do
  Aff.runAff_ (\_ -> pure unit) (mkdir filePath)
  buffer <- fromString content UTF8
  Aff.runAff_ (\_ -> pure unit) (writeFile (filePath <> "/" <> fileName) buffer)

makeExtenssion :: Output -> String
makeExtenssion = case _ of
  CommonJs -> ".js"
  CJs -> ".cjs"

compile :: String -> String -> Output -> Effect Unit
compile source outputPath output = do
  let
    program = runProgram source
  case program of
    Right program' -> case runSemantic program' of
      Right _ -> do
        for_ (codegen program')
          ( \(name /\ p) -> do
              writeOutput outputPath (name <> makeExtenssion output) (ppJsTree output p)
          )
      Left err -> logShow err
    Left err -> logShow err
