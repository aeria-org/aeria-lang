module Aeria.Syntax.Parser
  ( runCollectionP
  ) where

import Prelude hiding (between)

import Aeria.Syntax.Tree (Attribute(..), Collection(..), CollectionName(..), Condition(..), Getter(..), Getters(..), Macro(..), Name(..), Oper(..), Program(..), Properties, Property(..), PropertyName(..), Required, RequiredProperty(..), Table(..), Typ(..), Value(..))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (choice, many, manyTill, optionMaybe, sepBy, try, (<|>))
import Parsing.Language (emptyDef)
import Parsing.String (anyChar, char, eof, string)
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
      , reservedNames = ["collection", "properties", "required", "cond", "true", "false"]
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
        pTrue = lang.reserved "true" $> true

        pFalse :: Parser String Boolean
        pFalse = lang.reserved "false" $> false

    pVar :: Parser String Value
    pVar = do
      name <- lang.identifier
      pure $ VVar (Name name)

    pArray :: Parser String Value -> Parser String Value
    pArray p = VArray <$> lang.brackets go
      where
        go :: Parser String (List Value)
        go = sepBy (skipSpaces *> p <* skipSpaces) lang.comma

pOper :: Parser String Oper
pOper = lang.reservedOp "||" *> pure Or
  <|> lang.reservedOp "&&" *> pure And
  <|> lang.reservedOp ">" *> pure Gt
  <|> lang.reservedOp "<" *> pure Lt
  <|> lang.reservedOp "<=" *> pure Lte
  <|> lang.reservedOp ">=" *> pure Gte
  <|> lang.reservedOp "==" *> pure Eq

pCondition :: Parser String Condition
pCondition = Condition <$> pValue <*> pOper <*> pValue

pAttribute :: Parser String Attribute
pAttribute = do
  _ <- string "@"
  attributeName <- lang.identifier
  attributeValue <- lang.parens pValue
  pure $ Attribute (Name attributeName) attributeValue

pRequiredProperty :: Parser String RequiredProperty
pRequiredProperty = go
  where
    go :: Parser String RequiredProperty
    go = do
      propertyName <- pPropertyName
      condition <- optionMaybe pCondition'
      pure $ RequiredProperty propertyName condition

    pCondition' :: Parser String Condition
    pCondition' = do
      _ <- string "@"
      lang.reserved "cond"
      lang.parens pCondition

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

pGetter :: Parser String Getter
pGetter = do
  propertyName <- pPropertyName
  macroLang <- string "@" *> lang.identifier
  macroSource <- manyTill anyChar (lang.reserved "@end")
  pure $ Getter
    { propertyName,
      macro: Macro (Name macroLang) (fromCharArray <<< toUnfoldable $ macroSource)
    }

pProperties :: Parser String Properties
pProperties = fix \self ->
  let pProperty' = try (pProperty self)
  in lang.braces (many pProperty')

pRequired :: Parser String Required
pRequired =
  let pRequiredProperty' = try pRequiredProperty
  in lang.braces (many pRequiredProperty')

pTable :: Parser String Table
pTable =
  let pPropertyName' = try pPropertyName
  in lang.braces (Table <$> many pPropertyName')

pGetters :: Parser String Getters
pGetters =
  let pGetter' = try pGetter
  in lang.braces (many pGetter')

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
      required <- optionMaybe pRequired'
      properties <- pProperties'
      table <- optionMaybe pTable'
      getters <- optionMaybe pGetters'
      pure $ Collection
        { collectionName
        , properties
        , required
        , table
        , getters
        }

    pProperties' :: Parser String Properties
    pProperties' =  lang.reserved "properties" *> pProperties

    pRequired' :: Parser String Required
    pRequired' = lang.reserved "required" *> pRequired

    pTable' :: Parser String Table
    pTable' = lang.reserved "table" *> pTable

    pGetters' :: Parser String Getters
    pGetters' = lang.reserved "getters" *> pGetters

pProgram :: Parser String Program
pProgram = do
  collection <- pCollection
  pure $ Program { collection }

contents :: forall a. Parser String a -> Parser String a
contents p = lang.whiteSpace *> lang.lexeme p <* eof

runCollectionP :: String -> Either ParseError Program
runCollectionP s = runParser s (contents pProgram)
