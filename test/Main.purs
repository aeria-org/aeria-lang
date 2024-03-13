module Test.Main where

import Prelude

import Aeria.Syntax.Parser (runProgram)
import Data.Array (filterA, head, nub)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (isFile)
import Node.FS.Sync as S
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

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
    let schemaName = program <> ".schema"
    let goldenName = program <> ".golden"
    schema <- readFile (path <> "/" <> schemaName)
    golden <- readFile (path <> "/" <> goldenName)
    pure (Program { name: program, schema, golden })) programs

sanitize ∷ String -> String
sanitize str =  
  str
  # split (Pattern " ") 
  # joinWith ""
  # split (Pattern "\n") 
  # joinWith ""

test name schema golden = do 
  it name do
    let program = runProgram schema
    case program of
      Left err -> fail (show err)
      Right program' -> sanitize (show program') `shouldEqual` sanitize golden

main :: Effect Unit
main = do
  programs <- readPrograms "./test/Suite/Syntax"
  launchAff_ $ runSpec [consoleReporter] do
    describe "Syntax/Parser" do
      for_ programs (\(Program {name, schema, golden}) ->  
        test name schema golden)
