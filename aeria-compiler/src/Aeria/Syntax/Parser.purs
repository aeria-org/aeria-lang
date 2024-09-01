module Aeria.Syntax.Parser
  ( runParserProgram
  ) where

import Prelude hiding (between)

import Aeria.Diagnostic.Message (Diagnostic(..))
import Aeria.Diagnostic.Position (SourcePos(..), Span(..))
import Aeria.Syntax.Tree
import Control.Lazy (fix)
import Data.Array as A
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (traceM)
import Parsing (ParseError(..), Parser, Position(..), fail, position, runParser)
import Parsing.Combinators (choice, many, manyTill, optionMaybe, sepBy, try, (<?>), (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (emptyDef)
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (alphaNum, letter, oneOf, skipSpaces)
import Parsing.Token as P
import Unsafe.Coerce (unsafeCoerce)

type ParserM a = Parser String a

lang :: P.TokenParser
lang = P.makeTokenParser aeria
  where
  aeria :: P.LanguageDef
  aeria =
    P.LanguageDef
      (P.unGenLanguageDef emptyDef)
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = true
        , identStart = letter <|> oneOf [ '_' ]
        , identLetter = alphaNum <|> oneOf [ '_' ]
        , caseSensitive = true
        }

sourcePos :: ParserM SourcePos
sourcePos = do
  Position {column, index, line} <- position
  pure $ SourcePos index line column

pName :: forall a. (Span -> String -> a) -> ParserM a
pName constructor = do
  begin <- sourcePos
  ident <- lang.identifier <?> "an identifier"
  end <- sourcePos
  pure $ constructor (Span begin end) ident

pPropertyName :: ParserM PropertyName
pPropertyName = pName PropertyName

pFunctionName :: ParserM FunctionName
pFunctionName = pName FunctionName

pAttributeName :: ParserM AttributeName
pAttributeName = pName AttributeName

pCollectionName :: ParserM CollectionName
pCollectionName = pName CollectionName

pExtendsName :: ParserM ExtendsName
pExtendsName = do
  package <- lang.identifier
  _ <- char '.'
  collection <- lang.identifier
  pure $ ExtendsName package collection

pPropertyType :: ParserM CollectionProperties -> ParserM PropertyType
pPropertyType p = fix \self -> choice
  [ try (tArray self)
  , try tPrimitives
  , try tCollection
  , try (tObject self)
  ] <?> "a property type"
  where
    tPrimitives = choice [tConst, tStr, tBool, tInt, tNum, tEnum]
    tArray self = pArrayType self
    tCollection = pCollectionType
    tObject self = pObjectType self p

    pArrayType self = do
      begin <- sourcePos
      _ <- string "[]" <?> "\"[]\" for array type"
      arrType <- self
      end <- sourcePos
      pure $ PArray (Span begin end) arrType

    pCollectionType = do
      begin <- sourcePos
      collectionName <- pCollectionName
      end <- sourcePos
      pure $ PRef (Span begin end) collectionName

    pObjectType self p' = lang.braces $ do
      begin <- sourcePos
      results <- runParsers
        [ "required" /\ unsafeCoerce (pPropertyParser "required" pCollectionRequired)
        , "properties" /\ unsafeCoerce (pPropertyParser "properties" p')
        , "additionalProperties" /\ unsafeCoerce (pPropertyParser "additionalProperties" $ do
            propertyType <- self
            pure $ AdditionalProperties propertyType
          )
        ]
      let required = unsafeCoerce $ getParserValue "required" results
      let properties = unsafeCoerce $ getParserValue "properties" results
      let additionalProperties = unsafeCoerce $ getParserValue "additionalProperties" results
      end <- sourcePos
      pure $ PObject (Span begin end) required (fromMaybe L.Nil properties) additionalProperties

    tStr = pSimpleType PString "str"
    tBool = pSimpleType PBoolean "bool"
    tInt = pSimpleType PInteger "int"
    tNum = pSimpleType PNum "num"
    tEnum = pSimpleType PEnum "enum"
    tConst = pSimpleType PConst "const"

    pSimpleType constructor keyword = do
      begin <- sourcePos
      _ <- lang.reservedOp keyword <?> ("\"" <> keyword <> "\"")
      end <- sourcePos
      pure $ constructor (Span begin end)

pBoolean :: ParserM Boolean
pBoolean = pTrue <|> pFalse <?> "a boolean (\"true\" or \"false\")"
  where
    pTrue :: ParserM Boolean
    pTrue = lang.reserved "true" $> true

    pFalse :: ParserM Boolean
    pFalse = lang.reserved "false" $> false

pLiteral :: ParserM Literal
pLiteral = fix \self -> choice
  [ try pUndefined
  , try pNull
  , try pNum
  , try pInteger
  , try pString
  , try pBoolean'
  , try pProp
  , pArray self
  ] <?> "a literal value"
  where
    pInteger = pLiteralValue LInteger lang.integer
    pNum = pLiteralValue LNum lang.float
    pBoolean' = pLiteralValue LBoolean pBoolean
    pString = pLiteralValue LString lang.stringLiteral
    pProp = pLiteralValue LProperty pPropertyName

    pUndefined = do
      begin <- sourcePos
      lang.reserved "undefined"
      end <- sourcePos
      pure $ LUndefined (Span begin end)

    pNull = do
      begin <- sourcePos
      lang.reserved "null"
      end <- sourcePos
      pure $ LNull (Span begin end)

    pLiteralValue :: forall a. (Span -> a -> Literal) -> ParserM a -> ParserM Literal
    pLiteralValue constructor parser = do
      begin <- sourcePos
      value <- parser
      end <- sourcePos
      pure $ constructor (Span begin end) value

    pArray p = do
      begin <- sourcePos
      arrayLiteral <- lang.brackets (sepBy (skipSpaces *> p <* skipSpaces) lang.comma)
      end <- sourcePos
      pure $ LArray (Span begin end) arrayLiteral

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
      -- , [ unary "exists" EExists ]
      , [ unary "truthy" ETruthy ]
      , [ unary "!" ENot ]
      ]

    binary name fun assoc = Infix go assoc
      where
        go = do
          lang.reservedOp name <?> ("binary operator \"" <> name <> "\"")
          pure fun

    unary name fun = Prefix go
      where
        go = do
          lang.reservedOp name <?> ("unary operator \"" <> name <> "\"")
          pure fun

    expr self = lang.parens self <|> value

    value = ELiteral <$> pLiteral

pAttribute :: ParserM Attribute
pAttribute = do
  begin <- sourcePos
  attributeName <- lang.reservedOp "@" *> pAttributeName <?> "attribute name starting with \"@\""
  attributeValue <- case attributeName of
    AttributeName _ "constraints" -> do
      beginAttributeValue <- sourcePos
      expr <- lang.parens pExpr
      endAttributeValue <- sourcePos
      pure $ AExpr (Span beginAttributeValue endAttributeValue) expr
    _ -> do
      beginAttributeValue <- sourcePos
      literal <- optionMaybe $ lang.parens pLiteral
      endAttributeValue <- sourcePos
      let span = (Span beginAttributeValue endAttributeValue)
      case literal of
        Nothing -> pure $ ALiteral span (LBoolean span true)
        Just literal' -> pure $ ALiteral span literal'
  end <- sourcePos
  pure $ Attribute (Span begin end) attributeName attributeValue

pRequired :: ParserM Required
pRequired = go
  where
    go :: ParserM Required
    go = do
      begin <- sourcePos
      propertyName <- pPropertyName
      cond <- optionMaybe pCond
      end <- sourcePos
      pure $ Required (Span begin end) propertyName cond

pCond :: ParserM Cond
pCond = do
  lang.reserved "@cond"
  begin <- sourcePos
  expr <- lang.parens pExpr <?> "condition expression"
  end <- sourcePos
  pure (Cond (Span begin end) expr)

pProperty :: ParserM CollectionProperties -> ParserM Property
pProperty p = do
  begin <- sourcePos
  name <- pPropertyName
  type_ <- pPropertyType p
  attributes <- many pAttribute
  end <- sourcePos
  pure
    $ Property
        { span: Span begin end
        , name
        , type_
        , attributes
        }

pGetter :: ParserM Getter
pGetter = do
  begin <- sourcePos
  name <- pPropertyName
  macro <- pMacro "@js (doc) =>"
  end <- sourcePos
  pure
    $ Getter
        { span: (Span begin end)
        , name
        , macro
        }

pCollectionProperties :: ParserM CollectionProperties
pCollectionProperties =
  fix \self -> lang.braces $ do
    many (try (pProperty self))

pCollectionRequired :: ParserM CollectionRequired
pCollectionRequired = pListProperty pRequired

pListProperty :: forall a. ParserM a -> ParserM (L.List a)
pListProperty f = lang.braces (many (try f))

pCollectionTable :: ParserM CollectionTable
pCollectionTable = pListProperty pPropertyName

pCollectionTableMeta :: ParserM CollectionTableMeta
pCollectionTableMeta = pListProperty pPropertyName

pCollectionFunctions :: ParserM CollectionFunctions
pCollectionFunctions = lang.braces (many (try item))
  where
    item = do
      begin <- sourcePos
      functionName <- pFunctionName
      custom <- optionMaybe $ lang.reserved "?"
      attribute <- optionMaybe $ pAttribute
      end <- sourcePos
      pure $ FunctionItem
        { span: (Span begin end)
        , functionName
        , custom: (isJust custom)
        , expose: attribute
        }

pCollectionWritable :: ParserM CollectionWritable
pCollectionWritable = pListProperty pPropertyName

pCollectionForm :: ParserM CollectionForm
pCollectionForm = pListProperty pPropertyName

pCollectionFilters :: ParserM CollectionFilters
pCollectionFilters = pListProperty pPropertyName

pCollectionIndexes :: ParserM CollectionIndexes
pCollectionIndexes = pListProperty pPropertyName

pCollectionPresets :: ParserM CollectionPresets
pCollectionPresets = pListProperty lang.identifier

pCollectionGetters :: ParserM CollectionGetters
pCollectionGetters = lang.braces $ many (try pGetter)

pCollectionIcon :: ParserM CollectionIcon
pCollectionIcon = CollectionIcon <$> lang.stringLiteral

pCollectionOwned :: ParserM CollectionOwned
pCollectionOwned = pCollectionOwnedBoolean <|> pCollectionOwnedCustom
  where
    pCollectionOwnedBoolean = do
      start <- sourcePos
      value <- pBoolean
      end <- sourcePos
      pure $ CollectionOwnedBoolean (Span start end) value
    pCollectionOwnedCustom =  do
      start <- sourcePos
      value <- lang.stringLiteral
      end <- sourcePos
      pure $ CollectionOwnedCustom (Span start end) value

pCollectionTimestamps :: ParserM CollectionTimestamps
pCollectionTimestamps = CollectionTimestamps <$> pBoolean

pCollectionImmutable :: ParserM CollectionImmutable
pCollectionImmutable =
  try (CollectionImmutableBool <$> pBoolean)
  <|> try (CollectionImmutableList <$> pListProperty pPropertyName)

pCollectionSecurity :: ParserM CollectionSecurity
pCollectionSecurity = lang.braces do
  lang.reserved "functions"
  lang.braces $ many (try go)
  where
  go = do
    functionName <- pFunctionName
    lang.braces (pSecutiryItem functionName)

  pSecutiryItem name = do
    begin <- sourcePos
    results <- runParsers
      [ "rateLimiting" /\ (unsafeCoerce pRateLimiting')
      , "logging" /\ (unsafeCoerce pLogging')
      ]

    let rateLimiting = unsafeCoerce $ getParserValue "rateLimiting" results
    let logging = unsafeCoerce $ getParserValue "logging" results

    end <- sourcePos

    pure $ SecurityItem
      { span: Span begin end
      , functionName: name
      , rateLimiting
      , logging
      }

  pRateLimiting = lang.braces $ do
    begin <- sourcePos
    results <- runParsers
      [ "strategy" /\ (unsafeCoerce pStrategy)
      , "scale" /\ (unsafeCoerce pScale)
      ]
    end <- sourcePos
    let strategy = unsafeCoerce $ getParserValue "strategy" results
    let scale = unsafeCoerce $ getParserValue "scale" results
    pure $ SecurityRateLimiting { span: (Span begin end), strategy, scale  }

  pLogging = lang.braces do
    begin <- sourcePos
    strategy <- optionMaybe pStrategy
    end <- sourcePos
    pure $ SecurityLogging { span: (Span begin end), strategy }

  pRateLimiting' = pPropertyParser "rateLimiting" pRateLimiting
  pLogging' = pPropertyParser "logging" pLogging
  pStrategy = pPropertyParser "strategy" lang.stringLiteral
  pScale = pPropertyParser "scale" lang.integer

pCollectionSearch :: ParserM CollectionSearch
pCollectionSearch = lang.braces $ do
  results <- runParsers allParsers
  let placeholder = unsafeCoerce $ getParserValue "placeholder" results
  let indexes = unsafeCoerce $ getParserValue "indexes" results

  pure $ CollectionSearch
    { placeholder
    , indexes: fromMaybe L.Nil indexes
    }
  where
  allParsers =
    [ "indexes" /\ (unsafeCoerce pIndexs)
    , "placeholder" /\ (unsafeCoerce pPlaceholder)
    ]

  pIndexs = pPropertyParser "indexes" $ lang.braces (many (try pPropertyName))
  pPlaceholder = pPropertyParser "placeholder" lang.stringLiteral

pCollectionFiltersPresets :: ParserM CollectionFiltersPresets
pCollectionFiltersPresets = lang.braces $ many (try go)
  where
  go = do
    name <- pPropertyName
    lang.braces (pFiltersPresetsItem name)

  pFiltersPresetsItem name = do
    begin <- sourcePos
    results <- runParsers allParsers
    let label = unsafeCoerce $ getParserValue "name" results
    let badgeFunction = unsafeCoerce $ getParserValue "badgeFunction" results
    let filters = (unsafeCoerce $ getParserValue "filters" results)
    end <- sourcePos
    pure $ FiltersPresetsItem
      { span: (Span begin end)
      , label
      , name
      , badgeFunction
      , filters
      }

  allParsers =
    [ "name" /\ (unsafeCoerce pLabel)
    , "badgeFunction" /\ (unsafeCoerce pBadgeFunction)
    , "filters" /\ (unsafeCoerce pFilters)
    ]

  pLabel = pPropertyParser "name" lang.stringLiteral
  pBadgeFunction = pPropertyParser "badgeFunction" lang.stringLiteral
  pFilters = pPropertyParser "filters" (pMacro "@mongo")

pCollectionActions :: ParserM CollectionActions
pCollectionActions = lang.braces $ many (try pActionItem)

pCollectionIndividualActions :: ParserM CollectionIndividualActions
pCollectionIndividualActions = lang.braces $ many (try pActionItem)

pActionItem :: ParserM ActionItem
pActionItem = do
  name <- pPropertyName
  lang.braces (go name)
  where
  go actionName = do
    begin <- sourcePos
    results <- runParsers allParsers
    let label = unsafeCoerce $ getParserValue "label" results
    let icon = unsafeCoerce $ getParserValue "icon" results
    let ask = unsafeCoerce $ getParserValue "ask" results
    let selection = unsafeCoerce $ getParserValue "selection" results
    let effect = unsafeCoerce $ getParserValue "effect" results
    let button = unsafeCoerce $ getParserValue "button" results
    let translate = unsafeCoerce $ getParserValue "translate" results
    let setItem = unsafeCoerce $ getParserValue "setItem" results
    let fetchItem = unsafeCoerce $ getParserValue "fetchItem" results
    let clearItem = unsafeCoerce $ getParserValue "clearItem" results
    let params = unsafeCoerce $ getParserValue "params" results
    let query = unsafeCoerce $ getParserValue "query" results
    let requires = unsafeCoerce $ getParserValue "requires" results
    end <- sourcePos
    pure $ ActionItem
      { span: (Span begin end)
      , actionName
      , label
      , icon
      , ask
      , selection
      , effect
      , button
      , translate
      , setItem
      , fetchItem
      , clearItem
      , params
      , query
      , requires: fromMaybe L.Nil requires
      }

  pLabel = pPropertyParser "label" lang.stringLiteral
  pIcon = pPropertyParser "icon" lang.stringLiteral
  pAsk = pPropertyParser "ask" pBoolean
  pSelection = pPropertyParser "selection" pBoolean
  pEffect = pPropertyParser "effect" lang.stringLiteral
  pButton = pPropertyParser "button" pBoolean
  pTranslate = pPropertyParser "translate" pBoolean
  pSetItem = pPropertyParser "setItem" pBoolean
  pFetchItem = pPropertyParser "fetchItem" pBoolean
  pClearItem = pPropertyParser "clearItem" pBoolean
  pParams = pPropertyParser "params" (pMacro "@js () =>")
  pQuery = pPropertyParser "query" (pMacro "@js () =>")
  pRequires = pPropertyParser "requires" (pListProperty pPropertyName)

  allParsers =
    [ "label" /\ (unsafeCoerce pLabel)
    , "icon" /\ (unsafeCoerce pIcon)
    , "ask" /\ (unsafeCoerce pAsk)
    , "selection" /\ (unsafeCoerce pSelection)
    , "effect" /\ (unsafeCoerce pEffect)
    , "button" /\ (unsafeCoerce pButton)
    , "translate" /\ (unsafeCoerce pTranslate)
    , "setItem" /\ (unsafeCoerce pSetItem)
    , "fetchItem" /\ (unsafeCoerce pFetchItem)
    , "clearItem" /\ (unsafeCoerce pClearItem)
    , "params" /\ (unsafeCoerce pParams)
    , "query" /\ (unsafeCoerce pQuery)
    , "requires" /\ (unsafeCoerce pRequires)
    ]

pCollectionTableLayout :: ParserM CollectionTableLayout
pCollectionTableLayout = lang.braces $ many (try go)
  where
  go = do
    name <- pPropertyName
    lang.braces (pTableLayoutItem name)

  pTableLayoutItem actionName = do
    begin <- sourcePos
    results <- runParsers allParsers
    let route = unsafeCoerce $ getParserValue "route" results
    let icon = unsafeCoerce $ getParserValue "icon" results
    let ask = unsafeCoerce $ getParserValue "ask" results
    let if_ = unsafeCoerce $ getParserValue "if" results
    let selection = unsafeCoerce $ getParserValue "selection" results
    let effect = unsafeCoerce $ getParserValue "effect" results
    let button = unsafeCoerce $ getParserValue "button" results
    let translate = unsafeCoerce $ getParserValue "translate" results
    let setItem = unsafeCoerce $ getParserValue "setItem" results
    let fetchItem = unsafeCoerce $ getParserValue "fetchItem" results
    let clearItem = unsafeCoerce $ getParserValue "clearItem" results
    let params = unsafeCoerce $ getParserValue "params" results
    let query = unsafeCoerce $ getParserValue "query" results
    let requires = unsafeCoerce $ getParserValue "requires" results
    end <- sourcePos
    pure $ TableLayoutItem
      { span: (Span begin end)
      , actionName
      , route
      , if_
      , button: button
      , action: ActionItem
        { span: (Span begin end)
        , label: Nothing
        , button: Nothing
        , actionName
        , icon
        , ask
        , selection
        , effect
        , translate
        , setItem
        , fetchItem
        , clearItem
        , params
        , query
        , requires: fromMaybe L.Nil requires
        }
      }

  pIcon = pPropertyParser "icon" lang.stringLiteral
  pAsk = pPropertyParser "ask" pBoolean
  pSelection = pPropertyParser "selection" pBoolean
  pEffect = pPropertyParser "effect" lang.stringLiteral
  pTranslate = pPropertyParser "translate" pBoolean
  pSetItem = pPropertyParser "setItem" pBoolean
  pFetchItem = pPropertyParser "fetchItem" pBoolean
  pClearItem = pPropertyParser "clearItem" pBoolean
  pParams = pPropertyParser "params" (pMacro "@js () =>")
  pQuery = pPropertyParser "query" (pMacro "@js () =>")
  pRequires = pPropertyParser "requires" (pListProperty pPropertyName)
  pRoute = pPropertyParser "route" lang.stringLiteral
  pButton = pPropertyParser "button" $ try (Left <$> pBoolean) <|> try (Right <$> pCond)
  pIf = pPropertyParser "if" pCond

  allParsers =
    [ "route" /\ (unsafeCoerce pRoute)
    , "icon" /\ (unsafeCoerce pIcon)
    , "icon" /\ (unsafeCoerce pIcon)
    , "if" /\ (unsafeCoerce pIf)
    , "ask" /\ (unsafeCoerce pAsk)
    , "selection" /\ (unsafeCoerce pSelection)
    , "effect" /\ (unsafeCoerce pEffect)
    , "button" /\ (unsafeCoerce pButton)
    , "translate" /\ (unsafeCoerce pTranslate)
    , "setItem" /\ (unsafeCoerce pSetItem)
    , "fetchItem" /\ (unsafeCoerce pFetchItem)
    , "clearItem" /\ (unsafeCoerce pClearItem)
    , "params" /\ (unsafeCoerce pParams)
    , "query" /\ (unsafeCoerce pQuery)
    , "requires" /\ (unsafeCoerce pRequires)
    ]

pLayoutItem :: ParserM LayoutItem
pLayoutItem = do
  name <- pPropertyName
  lang.braces (go name)
  where
  go name = do
    begin <- sourcePos
    results <- runParsers allParsers
    let verticalSpacing = unsafeCoerce $ getParserValue "verticalSpacing" results
    let span_ = unsafeCoerce $ getParserValue "span" results
    let component = unsafeCoerce $ getParserValue "component" results
    let separator = unsafeCoerce $ getParserValue "separator" results
    let if_ = unsafeCoerce $ getParserValue "if" results
    end <- sourcePos

    pure $ LayoutItem
      { span: Span begin end
      , name
      , verticalSpacing
      , span_
      , component
      , separator
      , if_
      }

  allParsers =
    [ "verticalSpacing" /\ (unsafeCoerce pVerticalSpacing)
    , "span" /\ (unsafeCoerce pSpan)
    , "component" /\ (unsafeCoerce pComponent)
    , "separator" /\ (unsafeCoerce pSeparator)
    , "if" /\ (unsafeCoerce pIf)
    ]

  pVerticalSpacing = pPropertyParser "verticalSpacing" lang.float
  pComponent = pPropertyParser "component" pLayoutItemComponent
  pSeparator = pPropertyParser "separator" lang.stringLiteral
  pSpan = pPropertyParser "span" lang.float
  pIf = pPropertyParser "if" pCond

  pLayoutItemComponent = lang.braces $ do
    begin <- sourcePos
    results <- runParsers allParsers'
    let name = unsafeCoerce $ getParserValue "name" results
    let props = unsafeCoerce $ getParserValue "props" results
    end <- sourcePos
    pure $ LayoutItemComponent
      { span: Span begin end
      , name
      , props
      }
    where
      allParsers' =
        [ "name" /\ (unsafeCoerce pName')
        , "props" /\ (unsafeCoerce pProps)
        ]

      pName' = pPropertyParser "name" lang.stringLiteral
      pProps = pPropertyParser "props" (pMacro "@js () =>")

pCollectionLayout :: ParserM CollectionLayout
pCollectionLayout = lang.braces $ do
  begin <- sourcePos
  results <- runParsers allParsers'
  end <- sourcePos
  let name = unsafeCoerce $ getParserValue "name" results
  let options = unsafeCoerce $ getParserValue "options" results
  case name of
    Just name' ->
      pure $ CollectionLayout
        { span: Span begin end
        , name: name'
        , options
        }
    Nothing -> fail "'name' property in layout"

  where
    allParsers' =
      [ "name" /\ (unsafeCoerce pGrid)
      , "options" /\ (unsafeCoerce pOptions)
      ]

    pGrid = pPropertyParser "name" lang.stringLiteral
    pOptions = pPropertyParser "options" $ lang.braces do
      results <- runParsers allParsersOptions
      let title = unsafeCoerce $ getParserValue "title" results
      let badge = unsafeCoerce $ getParserValue "badge" results
      let picture = unsafeCoerce $ getParserValue "picture" results
      let information = unsafeCoerce $ getParserValue "information" results
      let active = unsafeCoerce $ getParserValue "active" results
      let translateBadge = unsafeCoerce $ getParserValue "translateBadge" results
      pure $ LayoutOptions
        { title
        , badge
        , picture
        , information
        , active
        , translateBadge
        }

    allParsersOptions =
      [ "title" /\ (unsafeCoerce pTitle)
      , "badge" /\ (unsafeCoerce pBadge)
      , "picture" /\ (unsafeCoerce pPicture)
      , "information" /\ (unsafeCoerce pInformation)
      , "active" /\ (unsafeCoerce pActive)
      , "translateBadge" /\ (unsafeCoerce pTranslateBadge)
      ]

    pTitle = pPropertyParser "title" pPropertyName
    pBadge = pPropertyParser "badge" pPropertyName
    pPicture = pPropertyParser "picture" pPropertyName
    pInformation = pPropertyParser "information" pPropertyName
    pActive = pPropertyParser "active" pPropertyName
    pTranslateBadge = pPropertyParser "translateBadge" pBoolean

pCollectionFormLayout :: ParserM CollectionFormLayout
pCollectionFormLayout = lang.braces $ do
  pPropertyParser "fields" $ lang.braces (many (try pLayoutItem))

pMacro :: String -> ParserM Macro
pMacro prefix = do
  begin <- sourcePos
  lang.reserved prefix
  code <- manyTill anyChar (lang.reserved "@end")
  end <- sourcePos
  pure $ Macro (Span begin end) (fromCharArray <<< L.toUnfoldable $ code)

pCollectionTemporary :: ParserM CollectionTemporary
pCollectionTemporary = lang.braces $ do
  index <- pPropertyParser "index" pPropertyName
  expireAfterSeconds <- pPropertyParser "expireAfterSeconds" lang.integer
  pure $ CollectionTemporary
    { index
    , expireAfterSeconds
    }

pPreferred :: ParserM CollectionPreferred
pPreferred = do
  lang.braces $ many (try go)
  where
    go = do
      name <- lang.identifier
      lang.braces (pPreferred' name)

    pPreferred' role = do
      begin <- sourcePos
      results <- runParsers allParsers
      let actions = unsafeCoerce $ getParserValue "actions" results
      let individualActions = unsafeCoerce $ getParserValue "individualActions" results
      let filters = unsafeCoerce $ getParserValue "filters" results
      let filtersPresets = unsafeCoerce $ getParserValue "filtersPresets" results
      let layout = unsafeCoerce $ getParserValue "layout" results
      let table = unsafeCoerce $ getParserValue "table" results
      let tableMeta = unsafeCoerce $ getParserValue "tableMeta" results
      let form = unsafeCoerce $ getParserValue "form" results
      let tableLayout = unsafeCoerce $ getParserValue "tableLayout" results
      let formLayout = unsafeCoerce $ getParserValue "formLayout" results
      end <- sourcePos
      pure $ PreferredItem
        { span: Span begin end
        , role
        , actions: fromMaybe L.Nil actions
        , individualActions: fromMaybe L.Nil individualActions
        , filters: fromMaybe L.Nil filters
        , filtersPresets: fromMaybe L.Nil filtersPresets
        , layout: layout
        , table: fromMaybe L.Nil table
        , tableMeta: fromMaybe L.Nil tableMeta
        , form: fromMaybe L.Nil form
        , tableLayout: fromMaybe L.Nil tableLayout
        , formLayout: fromMaybe L.Nil formLayout
        }

    allParsers =
      [ "actions"           /\ (unsafeCoerce pCollectionActions')
      , "individualActions" /\ (unsafeCoerce pCollectionIndividualActions')
      , "filters"           /\ (unsafeCoerce pCollectionFilters')
      , "filtersPresets"    /\ (unsafeCoerce pCollectionFiltersPresets')
      , "layout"            /\ (unsafeCoerce pCollectionLayout')
      , "table"             /\ (unsafeCoerce pCollectionTable')
      , "tableMeta"         /\ (unsafeCoerce pCollectionTableMeta')
      , "form"              /\ (unsafeCoerce pCollectionForm')
      , "tableLayout"       /\ (unsafeCoerce pCollectionTableLayout')
      , "formLayout"        /\ (unsafeCoerce pCollectionFormLayout')
      ]

    pCollectionActions'           = pPropertyParser "actions" pCollectionActions
    pCollectionIndividualActions' = pPropertyParser "individualActions" pCollectionIndividualActions
    pCollectionFilters'           = pPropertyParser "filters" pCollectionFilters
    pCollectionFiltersPresets'    = pPropertyParser "filtersPresets" pCollectionFiltersPresets
    pCollectionTableMeta'         = pPropertyParser "tableMeta" pCollectionTableMeta
    pCollectionTable'             = pPropertyParser "table" pCollectionTable
    pCollectionForm'              = pPropertyParser "form" pCollectionForm
    pCollectionLayout'            = pPropertyParser "layout" pCollectionLayout
    pCollectionFormLayout'        = pPropertyParser "formLayout" pCollectionFormLayout
    pCollectionTableLayout'       = pPropertyParser "tableLayout" pCollectionTableLayout

pCollection :: ParserM Collection
pCollection = go
  where
  go :: ParserM Collection
  go = do
    lang.reserved "collection"
    name <- pCollectionName
    extends' <- optionMaybe $ do
      lang.reserved "extends"
      pExtendsName
    lang.braces (pCollection' name extends')

  pCollection' :: CollectionName -> Maybe ExtendsName -> ParserM Collection
  pCollection' name extends = do
    begin <- sourcePos
    results <- runParsers allParsers
    let properties = unsafeCoerce $ getParserValue "properties" results
    let required = unsafeCoerce $ getParserValue "required" results
    let table = unsafeCoerce $ getParserValue "table" results
    let getters = unsafeCoerce $ getParserValue "getters" results
    let tableMeta = unsafeCoerce $ getParserValue "tableMeta" results
    let filters = unsafeCoerce $ getParserValue "filters" results
    let form = unsafeCoerce $ getParserValue "form" results
    let indexes = unsafeCoerce $ getParserValue "indexes" results
    let icon = unsafeCoerce $ getParserValue "icon" results
    let search = unsafeCoerce $ getParserValue "search" results
    let filtersPresets = unsafeCoerce $ getParserValue "filtersPresets" results
    let layout = unsafeCoerce $ getParserValue "layout" results
    let owned = unsafeCoerce $ getParserValue "owned" results
    let timestamps = unsafeCoerce $ getParserValue "timestamps" results
    let functions = unsafeCoerce $ getParserValue "functions" results
    let writable = unsafeCoerce $ getParserValue "writable" results
    let immutable = unsafeCoerce $ getParserValue "immutable" results
    let security = unsafeCoerce $ getParserValue "security" results
    let presets = unsafeCoerce $ getParserValue "presets" results
    let temporary = unsafeCoerce $ getParserValue "temporary" results
    let actions = unsafeCoerce $ getParserValue "actions" results
    let formLayout = unsafeCoerce $ getParserValue "formLayout" results
    let individualActions = unsafeCoerce $ getParserValue "individualActions" results
    let tableLayout = unsafeCoerce $ getParserValue "tableLayout" results
    let preferred = unsafeCoerce $ getParserValue "preferred" results
    end <- sourcePos
    pure
      $ Collection
          { span: (Span begin end)
          , name
          , extends
          , icon
          , owned
          , timestamps
          , search
          , immutable
          , temporary
          , required
          , preferred: fromMaybe L.Nil preferred
          , actions: fromMaybe L.Nil actions
          , formLayout: fromMaybe L.Nil formLayout
          , tableLayout: fromMaybe L.Nil tableLayout
          , presets: fromMaybe L.Nil presets
          , security: fromMaybe L.Nil security
          , functions: fromMaybe L.Nil functions
          , writable: fromMaybe L.Nil writable
          , properties: fromMaybe L.Nil properties
          , table: fromMaybe L.Nil table
          , getters: fromMaybe L.Nil getters
          , tableMeta: fromMaybe L.Nil tableMeta
          , filters: fromMaybe L.Nil filters
          , form: fromMaybe L.Nil form
          , indexes: fromMaybe L.Nil indexes
          , filtersPresets: fromMaybe L.Nil filtersPresets
          , individualActions: fromMaybe L.Nil individualActions
          , layout: layout
          }

  allParsers =
    [ "tableMeta"         /\ (unsafeCoerce pCollectionTableMeta')
    , "properties"        /\ (unsafeCoerce pCollectionProperties')
    , "required"          /\ (unsafeCoerce pCollectionRequired')
    , "filters"           /\ (unsafeCoerce pCollectionFilters')
    , "getters"           /\ (unsafeCoerce pCollectionGetters')
    , "indexes"           /\ (unsafeCoerce pCollectionIndexes')
    , "table"             /\ (unsafeCoerce pCollectionTable')
    , "form"              /\ (unsafeCoerce pCollectionForm')
    , "icon"              /\ (unsafeCoerce pCollectionIcon')
    , "search"            /\ (unsafeCoerce pCollectionSearch')
    , "filtersPresets"    /\ (unsafeCoerce pCollectionFiltersPresets')
    , "layout"            /\ (unsafeCoerce pCollectionLayout')
    , "owned"             /\ (unsafeCoerce pCollectionOwned')
    , "timestamps"        /\ (unsafeCoerce pCollectionTimestamps')
    , "functions"         /\ (unsafeCoerce pCollectionFunctions')
    , "writable"          /\ (unsafeCoerce pCollectionWritable')
    , "immutable"         /\ (unsafeCoerce pCollectionImmutable')
    , "security"          /\ (unsafeCoerce pCollectionSecurity')
    , "presets"           /\ (unsafeCoerce pCollectionPresets')
    , "temporary"         /\ (unsafeCoerce pCollectionTemporary')
    , "actions"           /\ (unsafeCoerce pCollectionActions')
    , "formLayout"        /\ (unsafeCoerce pCollectionFormLayout')
    , "individualActions" /\ (unsafeCoerce pCollectionIndividualActions')
    , "tableLayout"       /\ (unsafeCoerce pCollectionTableLayout')
    , "preferred"         /\ (unsafeCoerce pPreferred')
    ]

  pCollectionTableMeta'         = pPropertyParser "tableMeta" pCollectionTableMeta
  pCollectionProperties'        = pPropertyParser "properties" pCollectionProperties
  pCollectionRequired'          = pPropertyParser "required" pCollectionRequired
  pCollectionFilters'           = pPropertyParser "filters" pCollectionFilters
  pCollectionGetters'           = pPropertyParser "getters" pCollectionGetters
  pCollectionIndexes'           = pPropertyParser "indexes" pCollectionIndexes
  pCollectionTable'             = pPropertyParser "table" pCollectionTable
  pCollectionForm'              = pPropertyParser "form" pCollectionForm
  pCollectionIcon'              = pPropertyParser "icon" pCollectionIcon
  pCollectionSearch'            = pPropertyParser "search" pCollectionSearch
  pCollectionFiltersPresets'    = pPropertyParser "filtersPresets" pCollectionFiltersPresets
  pCollectionLayout'            = pPropertyParser "layout" pCollectionLayout
  pCollectionOwned'             = pPropertyParser "owned" pCollectionOwned
  pCollectionTimestamps'        = pPropertyParser "timestamps" pCollectionTimestamps
  pCollectionFunctions'         = pPropertyParser "functions" pCollectionFunctions
  pCollectionWritable'          = pPropertyParser "writable" pCollectionWritable
  pCollectionImmutable'         = pPropertyParser "immutable" pCollectionImmutable
  pCollectionSecurity'          = pPropertyParser "security" pCollectionSecurity
  pCollectionPresets'           = pPropertyParser "presets" pCollectionPresets
  pCollectionTemporary'         = pPropertyParser "temporary" pCollectionTemporary
  pCollectionActions'           = pPropertyParser "actions" pCollectionActions
  pCollectionIndividualActions' = pPropertyParser "individualActions" pCollectionIndividualActions
  pCollectionFormLayout'        = pPropertyParser "formLayout" pCollectionFormLayout
  pCollectionTableLayout'       = pPropertyParser "tableLayout" pCollectionTableLayout
  pPreferred'                   = pPropertyParser "preferred" pPreferred

getParserValue :: forall a. String -> Array (String /\ a) -> Maybe a
getParserValue key results =
  case A.find (\(key' /\ _) -> key' == key) results of
    Just (_ /\ v) -> Just v
    Nothing -> Nothing

runParsers ∷ forall a. Array (String /\ (ParserM a)) -> ParserM (Array (String /\ a))
runParsers ps = go' [] ps (A.length ps)
  where
  go' results _ 0  = pure results
  go' results parsers n  = do
    maybeResult <- choice' parsers
    case maybeResult of
      Just result ->
        go' (A.snoc results result) parsers (n - 1)
      Nothing ->
        go' results parsers (n - 1)

  choice' [] = pure Nothing
  choice' ps' =
    case A.uncons ps' of
      Just {head: (parserName /\ parser), tail} -> do
        x <- optionMaybe parser
        case x of
          Just a -> pure $ Just (parserName /\ a)
          Nothing -> choice' tail
      Nothing -> pure Nothing

pPropertyParser :: forall a. String -> ParserM a -> ParserM a
pPropertyParser key value = lang.reserved key *> value

pProgram :: ParserM Program
pProgram = do
  collections <- many pCollection
  pure $ Program { collections }

contents :: forall a. ParserM a -> ParserM a
contents p = lang.whiteSpace *> lang.lexeme p <* eof

runParserProgram :: String -> String -> Either Diagnostic Program
runParserProgram filepath source =
  case runParser source (contents pProgram) of
    Left (ParseError syntaxError (Position {index, line, column})) ->
      Left $ Diagnostic
        { filepath
        , source
        , info: syntaxError
        , span: (Span (SourcePos index line column) (SourcePos index line column))
        }
    Right program -> Right program
