module Aeria.Syntax.Parser
  ( runProgram
  ) where

import Prelude hiding (between)

import Aeria.Diagnostic.Message (Diagnostic(..), DiagnosticInfo(..))
import Aeria.Diagnostic.Position (SourcePos(..), Span(..))
import Aeria.Syntax.Error (SyntaxError(..))
import Aeria.Syntax.Tree (ActionItem(..), Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionActions, CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionFormLayout, CollectionFunctions, CollectionGetters, CollectionIcon(..), CollectionImmutable(..), CollectionIndexes, CollectionIndividualActions, CollectionLayout, CollectionName(..), CollectionOwned(..), CollectionPreferred, CollectionPresets, CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionSecurity, CollectionTable, CollectionTableLayout, CollectionTableMeta, CollectionTemporary(..), CollectionTimestamps(..), CollectionWritable, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), FunctionItem(..), FunctionName(..), Getter(..), ImmutableItem(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), Macro(..), PreferredItem(..), PresetItem(..), Program(..), Property(..), PropertyName(..), PropertyType(..), RequireItem(..), Required(..), SecurityItem(..), SecurityLogging(..), SecurityRateLimiting(..), TableItem(..), TableLayoutItem(..), TableMetaItem(..), WritableItem(..))
import Control.Lazy (fix)
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List, toUnfoldable)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (toLower)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (concatChar)
import Data.Tuple.Nested (type (/\), (/\))
import Parsing (ParseError(..), Parser, Position(..), position, runParser)
import Parsing.Combinators (choice, many, manyTill, optionMaybe, sepBy, try, (<|>))
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (emptyDef)
import Parsing.String (anyChar, eof, string)
import Parsing.String.Basic (alphaNum, letter, lower, oneOf, skipSpaces, upper)
import Parsing.Token as P
import Unsafe.Coerce (unsafeCoerce)

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
          , "search"
          , "placeholder"
          , "indexes"
          , "filters"
          , "filtersPresets"
          , "component"
          , "enum"
          , "cond"
          , "true"
          , "false"
          ]
        , identLetter = alphaNum <|> oneOf [ '_', '\'' ]
        , caseSensitive = true
        }

sourcePos :: ParserM SourcePos
sourcePos = do
  Position {column, index, line} <- position
  pure $ SourcePos index line column

pPropertyName :: ParserM PropertyName
pPropertyName = do
  begin <- sourcePos
  char <- lower
  rest <- lang.identifier
  end <- sourcePos
  pure (PropertyName (Span begin end) (concatChar char rest))

pFunctionName :: ParserM FunctionName
pFunctionName = do
  begin <- sourcePos
  char <- lower
  rest <- lang.identifier
  end <- sourcePos
  pure (FunctionName (Span begin end) (concatChar char rest))

pCollectionName :: ParserM CollectionName
pCollectionName = do
  begin <- sourcePos
  char <- upper
  rest <- lang.identifier
  end <- sourcePos
  pure (CollectionName (Span begin end) (toLower $ concatChar char rest))

pAttributeName :: ParserM AttributeName
pAttributeName = do
  begin <- sourcePos
  char <- lower
  rest <- lang.identifier
  end <- sourcePos
  pure (AttributeName (Span begin end) (concatChar char rest))

pPropertyType :: ParserM CollectionProperties -> ParserM PropertyType
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
    tStr
      <|> tBool
      <|> tInt
      <|> tFloat
      <|> tEnum

  tStr :: ParserM PropertyType
  tStr = do
    begin <- sourcePos
    _ <- lang.reservedOp "str"
    end <- sourcePos
    pure $ PString (Span begin end)

  tBool :: ParserM PropertyType
  tBool = do
    begin <- sourcePos
    _ <- lang.reservedOp "bool"
    end <- sourcePos
    pure $ PBoolean (Span begin end)

  tInt :: ParserM PropertyType
  tInt = do
    begin <- sourcePos
    _ <- lang.reservedOp "int"
    end <- sourcePos
    pure $ PInteger (Span begin end)

  tFloat :: ParserM PropertyType
  tFloat = do
    begin <- sourcePos
    _ <- lang.reservedOp "float"
    end <- sourcePos
    pure $ PFloat (Span begin end)

  tEnum :: ParserM PropertyType
  tEnum = do
    begin <- sourcePos
    _ <- lang.reservedOp "enum"
    end <- sourcePos
    pure $ PEnum (Span begin end)

  tCollection :: ParserM PropertyType
  tCollection = do
    begin <- sourcePos
    collectionName <- pCollectionName
    end <- sourcePos
    pure (PRef (Span begin end) collectionName)

  tArray :: ParserM PropertyType -> ParserM PropertyType
  tArray self = do
    begin <- sourcePos
    _ <- string "[]"
    arrType <- self
    end <- sourcePos
    pure (PArray (Span begin end) arrType)

  tObject :: ParserM PropertyType
  tObject = lang.braces $ do
    begin <- sourcePos
    results <- runParsers
      [ "required" /\ (unsafeCoerce $ pPropertyParser "required" pCollectionRequired)
      , "properties" /\ (unsafeCoerce $ pPropertyParser "properties" p)
      ]
    let required = unsafeCoerce $ getParserValue "required" results
    let properties = unsafeCoerce $ getParserValue "properties" results
    end <- sourcePos
    pure (PObject (Span begin end) (fromMaybe L.Nil required) (fromMaybe L.Nil properties))

pBoolean :: ParserM Boolean
pBoolean = pTrue <|> pFalse
  where
    pTrue :: ParserM Boolean
    pTrue = lang.reserved "true" $> true

    pFalse :: ParserM Boolean
    pFalse = lang.reserved "false" $> false

pLiteral :: ParserM Literal
pLiteral =
  fix \self ->
    choice
      [ try pFloat
      , try pInteger
      , try pString
      , try pBoolean'
      , try pProp
      , pArray self
      ]
  where
  pInteger :: ParserM Literal
  pInteger = do
    begin <- sourcePos
    integerLiteral <- lang.integer
    end <- sourcePos
    pure $ LInteger (Span begin end) integerLiteral

  pFloat :: ParserM Literal
  pFloat = do
    begin <- sourcePos
    floatLiteral <- lang.float
    end <- sourcePos
    pure $ LFloat (Span begin end) floatLiteral

  pString :: ParserM Literal
  pString = do
    begin <- sourcePos
    stringLiteral <- lang.stringLiteral
    end <- sourcePos
    pure $ LString (Span begin end) stringLiteral

  pBoolean' :: ParserM Literal
  pBoolean' = do
    begin <- sourcePos
    booleanLiteral <- pBoolean
    end <- sourcePos
    pure $ LBoolean (Span begin end) booleanLiteral

  pProp :: ParserM Literal
  pProp = do
    begin <- sourcePos
    propertyName <- pPropertyName
    end <- sourcePos
    pure $ LProperty (Span begin end) propertyName

  pArray :: ParserM Literal -> ParserM Literal
  pArray p = do
    begin <- sourcePos
    arrayLiteral <- lang.brackets go
    end <- sourcePos
    pure $ LArray (Span begin end) arrayLiteral
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
  begin <- sourcePos
  attributeName <- string "@" *> pAttributeName
  attributeValue <- case attributeName of
    AttributeName _ "constraints" -> do
      beginAttributeValue <- sourcePos
      expr <- lang.parens pExpr
      endAttributeValue <- sourcePos
      pure $ AExpr (Span beginAttributeValue endAttributeValue) expr
    _ -> do
      beginAttributeValue <- sourcePos
      literal <- lang.parens pLiteral
      endAttributeValue <- sourcePos
      pure $ ALiteral (Span beginAttributeValue endAttributeValue) literal
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
  expr <- lang.parens pExpr
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
        { span: (Span begin end)
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
  fix \self -> lang.braces $ many (try (pProperty self))

pCollectionRequired :: ParserM CollectionRequired
pCollectionRequired = lang.braces $ many (try pRequired)

pListProperty :: forall a. (Span -> PropertyName -> a) -> ParserM (L.List a)
pListProperty f = lang.braces (many (try item))
  where
    item = do
      begin <- sourcePos
      propertyName <- pPropertyName
      end <- sourcePos
      pure $ f (Span begin end) propertyName

pCollectionTable :: ParserM CollectionTable
pCollectionTable = pListProperty TableItem

pCollectionTableMeta :: ParserM CollectionTableMeta
pCollectionTableMeta = pListProperty TableMetaItem

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
pCollectionWritable = pListProperty WritableItem

pCollectionForm :: ParserM CollectionForm
pCollectionForm = pListProperty FormItem

pCollectionFilters :: ParserM CollectionFilters
pCollectionFilters = pListProperty FilterItem

pCollectionIndexes :: ParserM CollectionIndexes
pCollectionIndexes = pListProperty IndexesItem

pCollectionPresets :: ParserM CollectionPresets
pCollectionPresets = pListProperty PresetItem

pCollectionGetters :: ParserM CollectionGetters
pCollectionGetters = lang.braces $ many (try pGetter)

pCollectionIcon :: ParserM CollectionIcon
pCollectionIcon = CollectionIcon <$> lang.stringLiteral

pCollectionOwned :: ParserM CollectionOwned
pCollectionOwned = CollectionOwned <$> pBoolean

pCollectionTimestamps :: ParserM CollectionTimestamps
pCollectionTimestamps = CollectionTimestamps <$> pBoolean

pCollectionImmutable :: ParserM CollectionImmutable
pCollectionImmutable =
  try (CollectionImmutableBool <$> pBoolean)
  <|> try (CollectionImmutableList <$> pListProperty ImmutableItem)

pCollectionSecurity :: ParserM CollectionSecurity
pCollectionSecurity = lang.braces $ many (try go)
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
  pRequires = pPropertyParser "requires" (pListProperty RequireItem)

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
        , requires
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
  pRequires = pPropertyParser "requires" (pListProperty RequireItem)
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
        [ "name" /\ (unsafeCoerce pName)
        , "props" /\ (unsafeCoerce pProps)
        ]

      pName = pPropertyParser "name" lang.stringLiteral
      pProps = pPropertyParser "props" (pMacro "@js () =>")

pCollectionLayout :: ParserM CollectionLayout
pCollectionLayout = lang.braces $ many (try pLayoutItem)

pCollectionFormLayout :: ParserM CollectionFormLayout
pCollectionFormLayout = lang.braces $ many (try pLayoutItem)

pMacro :: String -> ParserM Macro
pMacro prefix = do
  begin <- sourcePos
  lang.reserved prefix
  code <- manyTill anyChar (lang.reserved "@end")
  end <- sourcePos
  pure $ Macro (Span begin end) (fromCharArray <<< toUnfoldable $ code)

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
        , layout: fromMaybe L.Nil layout
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
    lang.braces (pCollection' name)

  pCollection' :: CollectionName -> ParserM Collection
  pCollection' name = do
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
          , icon
          , owned
          , timestamps
          , search
          , immutable
          , temporary
          , preferred: fromMaybe L.Nil preferred
          , actions: fromMaybe L.Nil actions
          , formLayout: fromMaybe L.Nil formLayout
          , tableLayout: fromMaybe L.Nil tableLayout
          , presets: fromMaybe L.Nil presets
          , security: fromMaybe L.Nil security
          , functions: fromMaybe L.Nil functions
          , writable: fromMaybe L.Nil writable
          , properties: fromMaybe L.Nil properties
          , required: fromMaybe L.Nil required
          , table: fromMaybe L.Nil table
          , getters: fromMaybe L.Nil getters
          , tableMeta: fromMaybe L.Nil tableMeta
          , filters: fromMaybe L.Nil filters
          , form: fromMaybe L.Nil form
          , indexes: fromMaybe L.Nil indexes
          , filtersPresets: fromMaybe L.Nil filtersPresets
          , individualActions: fromMaybe L.Nil individualActions
          , layout: fromMaybe L.Nil layout
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
  pCollectionIndexes'           = pPropertyParser "indeParser" pCollectionIndexes
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

runParsers ∷ forall a. Array (String /\ (ParserM a)) → ParserM (Array (String /\ a))
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

runProgram :: String -> String -> Either Diagnostic Program
runProgram filepath source =
  case runParser source (contents pProgram) of
    Left (ParseError syntaxError (Position {index, line, column})) ->
      Left $ Diagnostic
        { filepath
        , source
        , info: DiagnosticSyntaxError (SyntaxError syntaxError)
        , span: (Span (SourcePos index line column) (SourcePos index line column))
        }
    Right program -> Right program
