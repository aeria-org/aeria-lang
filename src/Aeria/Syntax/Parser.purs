module Aeria.Syntax.Parser
  ( runProgram
  ) where

import Prelude hiding (between)
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), Collection(..), CollectionName(..), Expr(..), Getter(..), Getters, Literal(..), Macro(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Required, RequiredProperty(..), Table)
import Control.Lazy (fix)
import Data.Either (Either)
import Data.List (List(..), toUnfoldable)
import Data.Maybe (fromMaybe')
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (choice, many, manyTill, optionMaybe, sepBy, try, (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (emptyDef)
import Parsing.String (anyChar, eof, string)
import Parsing.String.Basic (alphaNum, letter, lower, oneOf, skipSpaces, upper)
import Parsing.Token as P

type ParserM a
  = Parser String a

lang :: P.TokenParser
lang = P.makeTokenParser aeria
  where
  aeria :: P.LanguageDef
  aeria =
    P.LanguageDef
      (P.unGenLanguageDef emptyDef)
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

pPropertyName :: ParserM PropertyName
pPropertyName = do
  char' <- lower
  rest <- lang.identifier
  pure (PropertyName (fromCharArray [ char' ] <> rest))

pCollectionName :: ParserM CollectionName
pCollectionName = do
  char' <- upper
  rest <- lang.identifier
  pure (CollectionName (fromCharArray [ char' ] <> rest))

pAttributeName :: ParserM AttributeName
pAttributeName = do
  char' <- lower
  rest <- lang.identifier
  pure (AttributeName (fromCharArray [ char' ] <> rest))

pPropertyType :: ParserM Properties -> ParserM PropertyType
pPropertyType p =
  fix \self ->
    choice
      [ try (tArray self)
      , try tPrimitives
      , try tCollection
      , try tObject
      ]
  where
  tPrimitives :: ParserM PropertyType
  tPrimitives =
    lang.reservedOp "str" *> pure PString
      <|> lang.reservedOp "bool" *> pure PBoolean
      <|> lang.reservedOp "int" *> pure PInteger
      <|> lang.reservedOp "float" *> pure PFloat
      <|> lang.reservedOp "enum" *> pure PEnum

  tCollection :: ParserM PropertyType
  tCollection = do
    name <- pCollectionName
    pure (PRef name)

  tArray :: ParserM PropertyType -> ParserM PropertyType
  tArray self = do
    _ <- string "[]"
    arrType <- self
    pure (PArray arrType)

  tObject :: ParserM PropertyType
  tObject = do
    properties <- p
    pure (PObject properties)

pLiteral :: ParserM Literal
pLiteral =
  fix \self ->
    choice
      [ try pFloat
      , try pInteger
      , try pString
      , try pBoolean
      , try pProp
      , pArray self
      ]
  where
  pInteger :: ParserM Literal
  pInteger = LInteger <$> lang.integer

  pFloat :: ParserM Literal
  pFloat = LFloat <$> lang.float

  pString :: ParserM Literal
  pString = LString <$> lang.stringLiteral

  pBoolean :: ParserM Literal
  pBoolean = LBoolean <$> (pTrue <|> pFalse)
    where
    pTrue :: ParserM Boolean
    pTrue = lang.reserved "true" $> true

    pFalse :: ParserM Boolean
    pFalse = lang.reserved "false" $> false

  pProp :: ParserM Literal
  pProp = do
    name <- pPropertyName
    pure $ LProperty name

  pArray :: ParserM Literal -> ParserM Literal
  pArray p = LArray <$> lang.brackets go
    where
    go :: ParserM (List Literal)
    go = sepBy (skipSpaces *> p <* skipSpaces) lang.comma

pExpr :: ParserM Expr
pExpr = fix \self -> buildExprParser table (expr self)
  where
  table =
    [ [ binary "==" EEq AssocLeft ]
    , [ binary "in" EIn AssocLeft ]
    , [ binary ">" EGt AssocLeft
      , binary "<" ELt AssocLeft
      , binary ">=" EGte AssocLeft
      , binary "<=" ELte AssocLeft
      ]
    , [ binary "&&" EAnd AssocLeft ]
    , [ binary "||" EOr AssocLeft ]
    , [ unary "exists" EExists ]
    , [ unary "!" ENot ]
    ]

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

  value = ELiteral <$> pLiteral

pAttribute :: ParserM Attribute
pAttribute = do
  _ <- string "@"
  attributeName <- pAttributeName
  attributeValue <- lang.parens pLiteral
  pure $ Attribute attributeName attributeValue

pRequiredProperty :: ParserM RequiredProperty
pRequiredProperty = go
  where
  go :: ParserM RequiredProperty
  go = do
    propertyName <- pPropertyName
    expr <- optionMaybe pExpr'
    pure $ RequiredProperty propertyName expr

  pExpr' :: ParserM Expr
  pExpr' = do
    _ <- string "@"
    lang.reserved "cond"
    lang.parens pExpr

pProperty :: ParserM Properties -> ParserM Property
pProperty p = do
  name <- pPropertyName
  type_ <- pPropertyType p
  attributes <- many pAttribute
  pure
    $ Property
        { name
        , type_
        , attributes
        }

pGetter :: ParserM Getter
pGetter = do
  name <- pPropertyName
  lang_ <- string "@" *> lang.identifier
  code <- manyTill anyChar (lang.reserved "@end")
  pure
    $ Getter
        { name
        , macro: Macro lang_ (fromCharArray <<< toUnfoldable $ code)
        }

pProperties :: ParserM Properties
pProperties =
  fix \self ->
    let
      pProperty' = try (pProperty self)
    in
      lang.braces (many pProperty')

pRequired :: ParserM Required
pRequired =
  let
    pRequiredProperty' = try pRequiredProperty
  in
    lang.braces (many pRequiredProperty')

pTable :: ParserM Table
pTable =
  let
    pPropertyName' = try pPropertyName
  in
    lang.braces (many pPropertyName')

pGetters :: ParserM Getters
pGetters =
  let
    pGetter' = try pGetter
  in
    lang.braces (many pGetter')

pCollection :: ParserM Collection
pCollection = go
  where
  go :: ParserM Collection
  go = do
    lang.reserved "collection"
    name <- pCollectionName
    lang.braces (pCollection' name)

  pCollection' :: CollectionName -> ParserM Collection
  pCollection' name = do
    required <- optionMaybe pRequired'
    properties <- pProperties'
    table <- optionMaybe pTable'
    getters <- optionMaybe pGetters'
    pure
      $ Collection
          { name
          , properties: properties
          , required: fromMaybe' (const Nil) required
          , table: fromMaybe' (const Nil) table
          , getters: fromMaybe' (const Nil) getters
          }

  pProperties' :: ParserM Properties
  pProperties' = lang.reserved "properties" *> pProperties

  pRequired' :: ParserM Required
  pRequired' = lang.reserved "required" *> pRequired

  pTable' :: ParserM Table
  pTable' = lang.reserved "table" *> pTable

  pGetters' :: ParserM Getters
  pGetters' = lang.reserved "getters" *> pGetters

pProgram :: ParserM Program
pProgram = do
  collections <- many pCollection
  pure $ Program { collections }

contents :: forall a. ParserM a -> ParserM a
contents p = lang.whiteSpace *> lang.lexeme p <* eof

runProgram :: String -> Either ParseError Program
runProgram s = runParser s (contents pProgram)
