module Main (main, module Aeria.Driver) where

import Prelude

import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Driver (compile, compile')
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
      case parseOutput output of
        Just output' -> compile schema outputPath output'
        Nothing -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"
    _ -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"

parseOutput :: String -> Maybe Output
parseOutput "commonjs" = Just CommonJs
parseOutput "esnext" = Just EsNext
parseOutput _ = Nothing
