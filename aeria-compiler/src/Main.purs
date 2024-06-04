module Main
  ( main
  , module Aeria.Driver
  , module Aeria.Diagnostic.Message
  , module Data.Either
  )
where

import Prelude

import Aeria.Diagnostic.Message (ppDiagnostic)
import Aeria.Driver (compile, compile')
import Data.Array (reverse, slice)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)

main :: Effect Unit
main = do
  args <- argv
  case slice 0 3 (reverse args) of
    [output, outputPath, schema] -> compile' schema outputPath output
    _ -> log "Usage: aeria-lang <filepath> <output> <commonjs|esnext>"
