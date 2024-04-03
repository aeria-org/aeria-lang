module Main where

import Prelude

import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Driver (compile)
import Data.Array (reverse, slice)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
  args <- argv
  case slice 0 3 (reverse args) of
    [output, outputPath, schema] -> do
      let output' = makeOutput output
      case output' of
        Just output'' -> do
          compile schema outputPath output''
        Nothing -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"
    _ -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"

makeOutput :: String -> Maybe Output
makeOutput "commonjs" = Just CommonJs
makeOutput "esnext" = Just EsNext
makeOutput _ = Nothing
