module Aeria.Syntax.Parser
  ( runCollectionP
  ) where

import Prelude hiding (between)

import Aeria.Syntax.Tree (Attribute(..), Collection(..), CollectionName(..), Expr(..), Getter(..), Getters, Macro(..), Name(..), Program(..), Properties, Property(..), PropertyName(..), Required, RequiredProperty(..), Table(..), PropertyType(..), Value(..))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (choice, many, manyTill, optionMaybe, sepBy, try, (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (emptyDef)
import Parsing.String (anyChar, eof, string)
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
      , reservedNames =
        [ "collection"
        , "properties"
        , "required"
        , "str"
        , "bool"
        , "int"
        , "float"
        , "file"
        , "enum"
        , "cond"
        , "true"
        , "false"
        ]
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

pPropertyType :: Parser String Properties -> Parser String PropertyType
pPropertyType p = fix \self ->
  choice
    [ try (tArray self)
    , try tPrimitives
    , try tCollection
    , try tObject
    ]
  where
    tPrimitives :: Parser String PropertyType
    tPrimitives = lang.reservedOp "str" *> pure PString
      <|> lang.reservedOp "bool" *> pure PBoolean
      <|> lang.reservedOp "int" *> pure PInteger
      <|> lang.reservedOp "float" *> pure PFloat
      <|> lang.reservedOp "file" *> pure PFile
      <|> lang.reservedOp "enum" *> pure PEnum

    tCollection :: Parser String PropertyType
    tCollection = do
      name <- pCollectionName
      pure (PCollection name)

    tArray :: Parser String PropertyType -> Parser String PropertyType
    tArray self = do
      _ <- string "[]"
      arrType <- self
      pure (PArray arrType)

    tObject :: Parser String PropertyType
    tObject = do
      properties <- p
      pure (PObject properties)

pValue :: Parser String Value
pValue = fix \self ->
  choice
    [ try pFloat
    , try pInt
    , try pString
    , try pBoolean
    , try pProp
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

    pProp :: Parser String Value
    pProp = do
      name <- lang.identifier
      pure $ VProperty (Name name)

    pArray :: Parser String Value -> Parser String Value
    pArray p = VArray <$> lang.brackets go
      where
        go :: Parser String (List Value)
        go = sepBy (skipSpaces *> p <* skipSpaces) lang.comma

pExpr :: Parser String Expr
pExpr = fix \self -> buildExprParser table (expr self)
  where
    table = [[binary "==" EEq AssocLeft],
             [binary "in" EIn AssocLeft],
             [binary ">" EGt AssocLeft,
              binary "<" ELt AssocLeft,
              binary ">=" EGte AssocLeft,
              binary "<=" ELte AssocLeft],
             [binary "&&" EAnd AssocLeft],
             [binary "||" EOr AssocLeft],
             [unary "exists" EExists],
             [unary "!" ENot]]

    binary name fun assoc = Infix go assoc
      where
        go = do
          lang.reservedOp name
          pure fun

    unary name fun = Prefix go
      where
        go = do
          lang.reservedOp name
          pure fun

    expr self = lang.parens self <|> value

    value = EValue <$> pValue

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
      expr <- optionMaybe pExpr'
      pure $ RequiredProperty propertyName expr

    pExpr' :: Parser String Expr
    pExpr' = do
      _ <- string "@"
      lang.reserved "cond"
      lang.parens pExpr

pProperty :: Parser String Properties -> Parser String Property
pProperty p = do
  propertyName <- pPropertyName
  propertyType <- pPropertyType p
  propertyAttributes <- many pAttribute
  pure $ Property
    { propertyName
    , propertyType
    , propertyAttributes
    }

pGetter :: Parser String Getter
pGetter = do
  getterName <- pPropertyName
  macroLang <- string "@" *> lang.identifier
  macroSource <- manyTill anyChar (lang.reserved "@end")
  pure $ Getter
    { getterName,
      getterMacro: Macro (Name macroLang) (fromCharArray <<< toUnfoldable $ macroSource)
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
      collectionRequired <- optionMaybe pRequired'
      collectionProperties <- pProperties'
      collectionTable <- optionMaybe pTable'
      collectionGetters <- optionMaybe pGetters'
      pure $ Collection
        { collectionName
        , collectionProperties
        , collectionRequired
        , collectionTable
        , collectionGetters
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
