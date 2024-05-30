module Test.Resolver where

import Prelude

import Data.Array (filterA, head, nub)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isFile)
import Node.FS.Sync as S

data Program = Program
  { name :: String
  , schema :: String
  , golden :: String }

getPrograms :: String -> Effect (Array String)
getPrograms dir = do
  entries <- S.readdir dir
  files <- filterA (isFile' dir) entries
  pure (nub $ map (\file -> fromMaybe "" $ head (split (Pattern ".") file)) files)

isFile' :: String -> String -> Effect Boolean
isFile' dir entry = do
  stats <- S.stat (dir <> "/" <> entry)
  pure (isFile stats)

readFile :: String -> Effect String
readFile filePath = do
  content <- S.readFile filePath
  toString UTF8 content

readPrograms :: String -> Effect (Array Program)
readPrograms path = do
  programs <- getPrograms path
  traverse (\program -> do
    let schemaName = program <> ".aeria"
    let goldenName = program <> ".golden"
    schema <- readFile (path <> "/" <> schemaName)
    golden <- readFile (path <> "/" <> goldenName)
    pure (Program { name: program, schema, golden })) programs
