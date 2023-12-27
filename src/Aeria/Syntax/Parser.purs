module Aeria.Syntax.Parser
  ( runP
  ) where

import Prelude

import Aeria.Syntax.Tree
  ( Attribute(..)
  , Collection(..)
  , CollectionName(..)
  , Name(..)
  , Program(..)
  , Properties
  , Property(..)
  , PropertyName(..)
  , Typ(..)
  , Value(..)
  )
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (choice, many, sepBy, try, (<|>))
import Parsing.Language (emptyDef)
import Parsing.String (eof, string)
import Parsing.String.Basic (alphaNum, letter, lower, oneOf, skipSpaces, upper)
import Parsing.Token as P

lang :: P.TokenParser
lang = P.makeTokenParser aeria
  where
    aeria :: P.LanguageDef
    aeria = P.LanguageDef (P.unGenLanguageDef emptyDef)
      { commentStart = "{-"
      , commentEnd = "-}"
      , commentLine = "--"
      , nestedComments = true
      , identStart = letter
      , reservedNames = ["collection", "properties"]
      , identLetter = alphaNum <|> oneOf [ '_', '\'' ]
      , caseSensitive = true
      }

pPropertyName :: Parser String PropertyName
pPropertyName = do
  char' <- lower
  rest <- lang.identifier
  pure (PropertyName (fromCharArray [char'] <> rest))

pCollectionName :: Parser String CollectionName
pCollectionName = do
  char' <- upper
  rest <- lang.identifier
  pure (CollectionName (fromCharArray [char'] <> rest))

pType :: Parser String Properties -> Parser String Typ
pType p = fix \self -> try (tArray self) <|> try tName <|> try tObject
  where
    tName :: Parser String Typ
    tName = do
      name <- lang.identifier
      pure (TName (Name name))

    tArray :: Parser String Typ -> Parser String Typ
    tArray self = do
      _ <- string "[]"
      arrType <- self
      pure (TArray arrType)

    tObject :: Parser String Typ
    tObject = do
      properties <- p
      pure (TObject properties)

pValue :: Parser String Value
pValue = fix \self ->
  choice
    [ try pFloat
    , try pInt
    , try pString
    , try pBoolean
    , try pVar
    , pArray self
    ]
  where
    pInt :: Parser String Value
    pInt = VInt <$> lang.integer

    pFloat :: Parser String Value
    pFloat = VFloat <$> lang.float

    pString :: Parser String Value
    pString = VString <$> lang.stringLiteral

    pBoolean :: Parser String Value
    pBoolean = VBoolean <$> (pTrue <|> pFalse)
      where
        pTrue :: Parser String Boolean
        pTrue = string "true" $> true

        pFalse :: Parser String Boolean
        pFalse = string "false" $> false

    pVar :: Parser String Value
    pVar = do
      name <- lang.identifier
      pure $ VVar (Name name)

    pArray :: Parser String Value -> Parser String Value
    pArray p = VArray <$> lang.brackets go
      where
        go :: Parser String (List Value)
        go = sepBy (skipSpaces *> p <* skipSpaces) lang.comma

pAttribute :: Parser String Attribute
pAttribute = do
  _ <- string "@"
  attributeName <- lang.identifier
  attributeValue <- lang.parens pValue
  pure $ Attribute (Name attributeName) attributeValue

pProperty :: Parser String Properties -> Parser String Property
pProperty p = do
  propertyName <- pPropertyName
  propertyType <- pType p
  propertyAttributes <- many pAttribute
  pure $ Property
    { propertyName
    , propertyType
    , propertyAttributes
    }

pProperties :: Parser String Properties
pProperties = fix \self ->
  let pProperty' = try (pProperty self)
  in lang.braces (many pProperty')

pCollection :: Parser String Collection
pCollection = go
  where
    go :: Parser String Collection
    go = do
      lang.reserved "collection"
      collectionName <- pCollectionName
      lang.braces (pCollection' collectionName)

    pCollection' :: CollectionName -> Parser String Collection
    pCollection' collectionName = do
      properties <- pProperties'
      pure $ Collection
        { collectionName
        , properties
        , required: Nothing
        , getters: Nothing
        , table: Nothing
        }

    pProperties' :: Parser String Properties
    pProperties' = do
      lang.reserved "properties"
      pProperties

pProgram :: Parser String Program
pProgram = do
  collection <- pCollection
  pure $ Program { collection }

contents :: forall a. Parser String a -> Parser String a
contents p = lang.whiteSpace *> lang.lexeme p <* eof

runP :: String -> Either ParseError Program
runP source = runParser source (contents pProgram)
