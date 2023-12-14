module Aeria.Syntax.Parser
  ( runP
  )
  where

import Prelude

import Data.Either (Either)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators ((<|>))
import Parsing.String (char)

ayebee :: Parser String Boolean
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure (b == 'B')

runP :: String -> Either ParseError Boolean
runP source = runParser source ayebee
