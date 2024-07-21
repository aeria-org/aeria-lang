module Aeria.Semantic where

import Prelude

import Aeria.Diagnostic.Message (Diagnostic(..))
import Aeria.Diagnostic.Position (Span)
import Aeria.Syntax.Tree
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List ((:))
import Data.List as L
import Data.Map.Internal (Map, empty, fromFoldable, insert, lookup) as M
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple.Nested ((/\))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

type SemanticM a
  = ReaderT Context (Except Diagnostic) a

data CollectionContext
  = CollectionContext
    { properties :: M.Map String Property
    , getters :: M.Map String Getter
    }

data Context
  = Context
    { filepath :: String
    , source :: String
    , collections :: M.Map String CollectionContext
    }

emptyContext :: String -> String -> Context
emptyContext filepath source = Context { collections: M.empty, filepath, source }

extendContext :: CollectionName -> CollectionProperties -> CollectionGetters -> Context -> SemanticM Context
extendContext (CollectionName _ collectionName) properties getters (Context { collections, source, filepath }) =
  case M.lookup collectionName collections of
    Just collectionContext -> do
      collectionContext' <- extendGetters collectionContext
      collectionContext'' <- extendProperties collectionContext'
      pure $ Context { collections: M.insert collectionName collectionContext'' collections, filepath, source }
    Nothing -> do
      collectionContext' <- extendProperties (CollectionContext { getters: M.empty, properties: M.empty })
      collectionContext'' <- extendGetters collectionContext'
      pure $ Context { collections: M.insert collectionName collectionContext'' collections, filepath, source }
  where
    extendProperties :: CollectionContext -> SemanticM CollectionContext
    extendProperties (CollectionContext { getters: gettersContext, properties: propertiesContext }) = do
      propertiesContext' <- L.foldM go propertiesContext properties
      pure $ CollectionContext { properties: propertiesContext', getters: gettersContext }
      where
        go pts property@(Property { name: (PropertyName span name) }) = do
          context <- ask
          case M.lookup name pts of
            Nothing -> pure $ M.insert name property pts
            Just _ -> do
              let diagnostic = makeDiagnostic context span ""
              throwError diagnostic

    extendGetters :: CollectionContext -> SemanticM CollectionContext
    extendGetters (CollectionContext { getters: gettersContext, properties: propertiesContext }) = do
      gettersContext' <- L.foldM go gettersContext getters
      pure $ CollectionContext { properties: propertiesContext, getters: gettersContext' }
      where
        go gts getter@(Getter { name: (PropertyName span name) }) = do
          context <- ask
          case M.lookup name gts of
            Nothing -> pure $ M.insert name getter gts
            Just _ -> do
              let diagnostic = makeDiagnostic context span ""
              throwError diagnostic

lookupProperty :: Context -> CollectionName -> PropertyName -> Maybe Property
lookupProperty (Context { collections }) (CollectionName _ collectionName) (PropertyName _ propertyName) = do
  case M.lookup collectionName collections of
    Just (CollectionContext { properties }) ->
      M.lookup propertyName properties
    Nothing -> Nothing

lookupGetter :: Context -> CollectionName -> PropertyName -> Maybe Getter
lookupGetter (Context { collections }) (CollectionName _ collectionName) (PropertyName _ propertyName) = do
  case M.lookup collectionName collections of
    Just (CollectionContext { getters }) ->
      M.lookup propertyName getters
    Nothing -> Nothing

lookupCollection :: Context -> CollectionName -> Maybe CollectionContext
lookupCollection (Context { collections }) (CollectionName _ collectionName) = M.lookup collectionName collections

hasPropertyOrGetter :: Context -> CollectionName -> PropertyName -> Maybe Unit
hasPropertyOrGetter context collectionName propertyName@(PropertyName _ propertyName') = do
  when (propertyName' /= "_id") do
    case lookupGetter context collectionName propertyName of
      Nothing ->
        case lookupProperty context collectionName propertyName of
          Nothing -> Nothing
          Just _ -> Just unit
      Just _ -> Just unit

hasProperty :: Context -> CollectionName -> PropertyName -> Maybe Unit
hasProperty context collectionName propertyName@(PropertyName _ propertyName') = do
  when (propertyName' /= "_id") do
    case lookupProperty context collectionName propertyName of
      Nothing -> Nothing
      Just _ -> Just unit

makeDiagnostic :: Context -> Span -> String -> Diagnostic
makeDiagnostic (Context { filepath, source }) span semanticError =
  Diagnostic
    { filepath
    , span
    , source
    , info: semanticError
    }

throwDiagnostic :: Span -> String -> SemanticM Unit
throwDiagnostic span semanticError = do
  context <- ask
  let diagnostic = makeDiagnostic context span semanticError
  throwError diagnostic

hasProperties :: CollectionName -> L.List PropertyName -> SemanticM Unit
hasProperties collectionName@(CollectionName _ collectionName') = traverse_ \propertyName@(PropertyName span propertyName') -> do
  context <- ask
  case hasPropertyOrGetter context collectionName propertyName of
    Just _  -> pure unit
    Nothing -> throwDiagnostic span $ "Property \"" <> propertyName' <> "\" does not exist on collection \"" <> collectionName' <> "\""

sExpr :: Context -> CollectionName -> Expr -> Either String Unit
sExpr context collectionName@(CollectionName _ collectionName') expr =
  case expr of
    EIn lft rgt   -> sBinaryExpr lft rgt
    EEq lft rgt   -> sBinaryExpr lft rgt
    ELt lft rgt   -> sBinaryExpr lft rgt
    EGt lft rgt   -> sBinaryExpr lft rgt
    ELte lft rgt  -> sBinaryExpr lft rgt
    EGte lft rgt  -> sBinaryExpr lft rgt
    EOr lft rgt   -> sBinaryExpr lft rgt
    EAnd lft rgt  -> sBinaryExpr lft rgt
    ENot expr'    -> sUnaryExpr expr'
    ETruthy expr' -> sUnaryExpr expr'
    ELiteral _    -> pure unit
    -- EExists expr' -> sBinaryExpr expr'
  where
    sUnaryExpr :: Expr -> Either String Unit
    sUnaryExpr (ELiteral (LProperty _ propertyName)) = hasProperty' propertyName
    sUnaryExpr _ = Left "Expected a property name in the expression"

    sBinaryExpr :: Expr -> Expr -> Either String Unit
    sBinaryExpr (ELiteral (LProperty _ propertyName)) rgt = do
      hasProperty' propertyName
      sExpr context collectionName rgt
    sBinaryExpr _ _ = Left "Expected a property name to the left of the expression"

    hasProperty' :: PropertyName -> Either String Unit
    hasProperty' propertyName@(PropertyName _ name) =
      when (isNothing (hasProperty context collectionName propertyName)) (Left $ "Property \"" <> name <> "\" does not exist on collection \"" <> collectionName' <> "\"")

sCollection :: Collection -> SemanticM Context
sCollection (Collection
  { name
  , properties
  , getters
  , table
  , tableMeta
  , filters
  , indexes
  , form
  , required
  , search
  , filtersPresets
  , layout
  , preferred
  , tableLayout
  , security
  , functions
  , formLayout
  , actions
  , individualActions
  , writable
  , immutable
  }) = do
  context <- ask >>= extendContext name properties getters
  local (const context) $ do
    sProperties name properties
    sRequired name required
    sGetters name getters
    sTable name table
    sTableMeta name tableMeta
    sForm name form
    sFilters name filters
    sIndexes name indexes
    sTableLayout name tableLayout
    sLayout name layout
    sFiltersPresets name filtersPresets
    sWritable name writable
    sFunctions name functions
    sSecurity functions security
    sActions name actions
    sIndividualActions name individualActions
    sFormLayout name formLayout
    sImmutable name immutable
    sPreferred name preferred
    case search of
      Just search' -> sSearch name search'
      Nothing -> pure unit
    ask

sFiltersPresets :: CollectionName -> CollectionFiltersPresets -> SemanticM Unit
sFiltersPresets _ = traverse_ go
  where
  go (FiltersPresetsItem { span, filters }) = do
    when (isNothing filters) (throwDiagnostic span "\"filters\" property in \"filtersPresets\" is required")

sFormLayout :: CollectionName -> CollectionFormLayout -> SemanticM Unit
sFormLayout _ = traverse_ go
  where
  go (LayoutItem { component: Nothing }) = pure unit
  go (LayoutItem { component: (Just (LayoutItemComponent { span, name })) }) =
    when (isNothing name) (throwDiagnostic span "\"name\" property in \"formLayout\" is required")

sIndividualActions :: CollectionName -> CollectionIndividualActions -> SemanticM Unit
sIndividualActions _ = traverse_ go
  where
  go (ActionItem { span, label }) =
    when (isNothing label) (throwDiagnostic span "\"label\" property in \"individualActions\" is required")

sActions :: CollectionName -> CollectionActions -> SemanticM Unit
sActions _ = traverse_ go
  where
  go (ActionItem { span, label }) =
    when (isNothing label) (throwDiagnostic span "\"label\" property in \"actions\" is required")

sTableLayout :: CollectionName -> CollectionTableLayout -> SemanticM Unit
sTableLayout collectionName = traverse_ go
  where
  go (TableLayoutItem
    { span
    , button
    }) = do
      context <- ask
      case button of
        Just (Right (Cond _ cond)) ->
          case sExpr context collectionName cond of
            Left err -> throwDiagnostic span err
            Right _ -> pure unit
        _ -> pure unit

sLayout :: CollectionName -> Maybe CollectionLayout -> SemanticM Unit
sLayout _ Nothing = pure unit
sLayout collectionName (Just (CollectionLayout {span, name, options})) = do
  when (not (name `elem` ["grid", "tabular", "list"])) $ (throwDiagnostic span "Invalid layout name")
  case options of
    Just (LayoutOptions { title, badge, picture, information, active }) -> do
      let properties = L.fromFoldable [title, badge, picture, information, active]
      hasProperties collectionName (L.catMaybes properties)
    Nothing -> pure unit

sPreferred :: CollectionName -> CollectionPreferred -> SemanticM Unit
sPreferred collectionName = traverse_ go
  where
  go (PreferredItem
    { tableMeta
    , actions
    , individualActions
    , filters
    , filtersPresets
    , layout
    , table
    , form
    , tableLayout
    , formLayout
    }) = do
      sTableMeta collectionName tableMeta
      sActions collectionName actions
      sIndividualActions collectionName individualActions
      sFilters collectionName filters
      sFiltersPresets collectionName filtersPresets
      sLayout collectionName layout
      sTable collectionName table
      sForm collectionName form
      sTableLayout collectionName tableLayout
      sFormLayout collectionName formLayout

sSearch :: CollectionName -> CollectionSearch -> SemanticM Unit
sSearch collectionName (CollectionSearch { indexes }) = hasProperties collectionName indexes

sSecurity :: CollectionFunctions -> CollectionSecurity -> SemanticM Unit
sSecurity functions = traverse_ go
  where
  go (SecurityItem {span, functionName, rateLimiting, logging}) = do
    sFunctionName span functionName
    sRateLimiting rateLimiting
    sLogging logging
    pure unit

  sFunctionName span (FunctionName _ functionName) =
    case L.find (\(FunctionItem { functionName: (FunctionName _ function) }) -> function == functionName) functions of
      Just _ -> pure unit
      Nothing -> throwDiagnostic span
        ("Cannot find function \"" <> functionName <> "\"")

  sRateLimiting (Just (SecurityRateLimiting {span, strategy: (Just strategy)})) =
      if strategy `elem` ["tenant", "ip"]
      then pure unit
      else throwDiagnostic span $ "Value \""<> strategy <> "\" is not assignable to strategy"
  sRateLimiting _ = pure unit

  sLogging (Just (SecurityLogging { span, strategy: (Just strategy) })) =
    if strategy `elem` ["tenant", "ip"]
      then pure unit
      else throwDiagnostic span $ "Value \""<> strategy <> "\" is not assignable to strategy"
  sLogging _ = pure unit

sRequired :: CollectionName -> CollectionRequired -> SemanticM Unit
sRequired collectionName@(CollectionName _ collectionName') = traverse_ go
  where
    go (Required _ propertyName@(PropertyName span name) cond) = do
      context <- ask
      when (isNothing (lookupProperty context collectionName propertyName))
        (throwDiagnostic span $ "Property \"" <> name <> "\" does not exist on collection \"" <> collectionName' <> "\"")
      sCond context cond

    sCond context (Just (Cond span expr)) =
      case sExpr context collectionName expr of
        Right unit -> pure unit
        Left exprError -> throwDiagnostic span exprError
    sCond _ Nothing = pure unit

sTable :: CollectionName -> CollectionTable -> SemanticM Unit
sTable = hasProperties

sTableMeta :: CollectionName -> CollectionTableMeta -> SemanticM Unit
sTableMeta = hasProperties

sFilters :: CollectionName -> CollectionFilters -> SemanticM Unit
sFilters = hasProperties

sForm :: CollectionName -> CollectionForm -> SemanticM Unit
sForm = hasProperties

sIndexes :: CollectionName -> CollectionIndexes -> SemanticM Unit
sIndexes = hasProperties

sWritable :: CollectionName -> CollectionWritable -> SemanticM Unit
sWritable = hasProperties

sFunctions :: CollectionName -> CollectionFunctions -> SemanticM Unit
sFunctions _ collectionFunctions =
  traverse_ (\(FunctionItem { expose }) ->
    case expose of
      Just (Attribute _ (AttributeName _ "expose") (ALiteral _ (LArray _ _))) -> pure unit
      Just (Attribute _ (AttributeName _ "expose") (ALiteral _ (LBoolean _ _))) -> pure unit
      Just (Attribute _ (AttributeName _ "expose") (ALiteral _ (LString _ _))) -> pure unit
      Just (Attribute span (AttributeName _ name) _) ->
        throwDiagnostic span $ "Attribute \"" <> name <> "\" does not exist on functions"
      Nothing -> pure unit
  ) collectionFunctions

sImmutable :: CollectionName -> Maybe CollectionImmutable -> SemanticM Unit
sImmutable _ Nothing = pure unit
sImmutable _ (Just (CollectionImmutableBool _)) = pure unit
sImmutable collectionName (Just (CollectionImmutableList immutable)) = hasProperties collectionName immutable

sGetters :: CollectionName -> CollectionGetters -> SemanticM Unit
sGetters collectionName = traverse_ \(Getter { name: propertyName@(PropertyName span name) }) -> do
  context <- ask
  when (isJust (lookupProperty context collectionName propertyName))
    (throwDiagnostic span $ "Property \"" <> name <> "\" is already defined")

sProperties :: CollectionName -> CollectionProperties -> SemanticM Unit
sProperties collectionName = traverse_ (sProperty collectionName)

sProperty :: CollectionName -> Property -> SemanticM Unit
sProperty collectionName property@(Property { type_ }) =
  case type_ of
    PBoolean _ -> sBooleanProperty property
    PArray _ _ -> sArrayProperty collectionName property
    PObject _ _ _ _ -> sObjectProperty collectionName property
    PEnum _ -> sEnumProperty property
    PString _ -> sStringProperty property
    PNum _ -> sNumberProperty property
    PConst _ -> sConstProperty property
    PInteger _ -> sNumberProperty property
    PRef _ (CollectionName _ "File") -> sFileProperty property
    PRef _ ref -> sRefProperty ref property

sConstProperty :: Property -> SemanticM Unit
sConstProperty = sAttributes'
  where
    sAttributes' property' = sAttributes property' literalAttributes M.empty
    literalAttributes =
      M.fromFoldable
        [ "value" /\ sType [TBoolean, TNum, TInteger, TString, TUndefined, TNull]
        ]

sBooleanProperty :: Property -> SemanticM Unit
sBooleanProperty = sAttributes'
  where
    sAttributes' property' = sAttributes property' literalAttributes M.empty

    literalAttributes =
      M.fromFoldable
        [ "default" /\ sType [TBoolean]
        ]

sArrayProperty :: CollectionName -> Property -> SemanticM Unit
sArrayProperty collectionName = sAttributes'
  where
    sAttributes' (Property { span, name, type_: (PArray span' type'), attributes }) = do
      let arrayAttributes = L.filter (\(Attribute _ (AttributeName _ attributeName) _) -> isArrayAttribute attributeName) attributes
      let typeAttributes = L.filter (\(Attribute _ (AttributeName _ attributeName) _) -> not $ isArrayAttribute attributeName) attributes

      sAttributes (Property { span, name, type_: (PArray span' type'), attributes: arrayAttributes }) literalAttributes M.empty
      case type' of
        object@(PObject _ _ _ _)  -> sObjectProperty collectionName (Property { span, type_: object, attributes: typeAttributes, name })
        _                       -> sProperty collectionName (Property { span, type_: type', attributes: typeAttributes, name })
    sAttributes' _ = unsafePerformEffect (throw "unresearchable")

    literalAttributes =
      M.fromFoldable
        [ "default" /\ sType [TArray]
        , "minItems" /\ sType [TInteger]
        , "maxItems" /\ sType [TInteger]
        , "uniqueItems" /\ sType [TBoolean]
        ]

    isArrayAttribute attribute =
      case M.lookup attribute literalAttributes of
        Just _ -> true
        Nothing -> false

sObjectProperty :: CollectionName -> Property -> SemanticM Unit
sObjectProperty (CollectionName _ collectionName) = sAttributes' 0
  where
    sAttributes' idx property@(Property { span, type_ }) = do
      sAttributes property M.empty M.empty
      case type_ of
        PObject _ required properties _ -> do
          let objectName =  (CollectionName span (collectionName <> (show idx)))
          context <- ask >>= extendContext objectName properties L.Nil
          local (const context) $ do
            sRequired objectName required
            traverse_ (sProperty objectName) properties
        _ -> unsafePerformEffect (throw "unresearchable")

sRefProperty :: CollectionName -> Property -> SemanticM Unit
sRefProperty collectionName@(CollectionName span collectionName') property = do
  context <- ask
  when (isNothing (lookupCollection context collectionName))
    (throwDiagnostic span ("Cannot find collection \"" <> collectionName' <> "\""))
  sAttributes' property
  where
    sAttributes' property' = sAttributes property' literalAttributes exprAttributes

    literalAttributes =
      M.fromFoldable
        [ "indexes" /\ sArrayType'
        , "populate" /\ sArrayType'
        , "inline" /\ sType [TBoolean]
        ]

    exprAttributes =
      M.fromFoldable
        ["constraints" /\ sConstraints
        ]

    sArrayType' property' literal@(LArray _ values) = do
      sArrayType TProperty property' literal
      traverse_ hasPropertyOrGetter' values
    sArrayType' _ literal =
      throwDiagnostic (literalPos literal) ("Attribute \"indexes\" must be an array of property names")

    hasPropertyOrGetter' (LProperty span' propertyName) = do
      context <- ask
      case hasPropertyOrGetter context collectionName propertyName of
        Nothing ->
          throwDiagnostic span' ""
        Just _ -> pure unit
    hasPropertyOrGetter' literal =
      throwDiagnostic (literalPos literal) "Expected property name"

    sConstraints _property' expr = do
      context <- ask
      case sExpr context collectionName expr of
        Right unit -> pure unit
        Left exprError -> throwDiagnostic span exprError

sFileProperty :: Property -> SemanticM Unit
sFileProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "accept" /\ sArrayType TString
      ]

sType :: Array Typ -> Property -> Literal -> SemanticM Unit
sType accepts _ literal = do
  let type' = typeOf literal
  let span = literalPos literal
  when (not (type' `elem` accepts)) (throwDiagnostic span "Type mismatch")

sArrayType :: Typ -> Property -> Literal -> SemanticM Unit
sArrayType expected _ (LArray _ arr@(l:_)) =
  case typeOfArray arr of
    Just arrType ->
      when (arrType /= expected) $
        throwDiagnostic (literalPos l) ("Array type mismatch, expected array of " <> show expected <> "but received " <> show arrType)
    Nothing ->
      throwDiagnostic (literalPos l) ("Array type mismatch, expected array of " <> show expected <> "but received " <> show (typeOf l))
sArrayType expected _ literal =
  throwDiagnostic (literalPos literal)
    ("Array type mismatch, expected array of " <> show expected <> "but received " <> show (typeOf literal))

sEnumProperty :: Property -> SemanticM Unit
sEnumProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "values" /\ sArrayType TString
      , "default" /\ sType [TString]
      ]

sNumberProperty :: Property -> SemanticM Unit
sNumberProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "minimum" /\ sType [TNum, TInteger]
      , "maximum" /\ sType [TNum, TInteger]
      , "exclusiveMinimum" /\ sType [TNum, TInteger]
      , "exclusiveMaximum" /\ sType [TNum, TInteger]
      , "default" /\ sType [TInteger, TNum]
      ]

typeOfArray :: L.List Literal -> Maybe Typ
typeOfArray L.Nil = Nothing
typeOfArray (a : as) = go as (typeOf a)
  where
  go L.Nil t = Just t
  go (x : xs) t
    | typeOf x /= t = Nothing
    | otherwise = go xs t

typeOf :: Literal -> Typ
typeOf (LInteger _ _) = TInteger
typeOf (LNum _ _) = TNum
typeOf (LString _ _) = TString
typeOf (LBoolean _ _) = TBoolean
typeOf (LArray _ _) = TArray
typeOf (LProperty _ _) = TProperty
typeOf (LUndefined _) = TUndefined
typeOf (LNull _) = TNull

literalPos :: Literal -> Span
literalPos (LUndefined span) = span
literalPos (LNull span) = span
literalPos (LInteger span _) = span
literalPos (LNum span _) = span
literalPos (LString span _) = span
literalPos (LBoolean span _) = span
literalPos (LArray span _) = span
literalPos (LProperty span _) = span

sStringProperty :: Property -> SemanticM Unit
sStringProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes :: M.Map String (Property -> Literal -> SemanticM Unit)
  literalAttributes =
    M.fromFoldable
      [ "minLength" /\ sType [TInteger]
      , "maxLength" /\ sType [TInteger]
      , "format" /\ checkFormat
      , "inputType" /\ checkInputType
      , "mask" /\ checkMask
      , "default" /\ sType [TString]
      ]

  checkFormat :: Property -> Literal -> SemanticM Unit
  checkFormat _ (LString span value)
    | elem value formatOptions = pure unit
    | otherwise = throwDiagnostic span "Atribute \"format\" must be one of: \"date\" or \"date-time\""
  checkFormat _ literal =
    throwDiagnostic (literalPos literal) ("Atribute \"format\" must be a string")

  checkInputType :: Property -> Literal -> SemanticM Unit
  checkInputType _ (LString span value)
    | elem value typeOptions = pure unit
    | otherwise = throwDiagnostic span "Attribute \"inputType\" must be one of: \"text\", \"email\", \"password\", \"search\", \"time\" or \"month\""
  checkInputType _ literal =
    throwDiagnostic (literalPos literal) ("Atribute \"inputType\" must be a string")

  checkMask :: Property -> Literal -> SemanticM Unit
  checkMask property' array@(LArray _ _) = sArrayType TString property' array
  checkMask _ (LString _ _) = pure unit
  checkMask _ literal =
    throwDiagnostic (literalPos literal) ("Attribute \"mask\" must be a string or an array of strings")

  typeOptions :: Array String
  typeOptions = [ "text", "email", "password", "search", "time", "month" ]

  formatOptions :: Array String
  formatOptions = [ "date", "date-time" ]

sAttributes ::
  Property ->
  M.Map String (Property -> Literal -> SemanticM Unit) ->
  M.Map String (Property -> Expr -> SemanticM Unit) ->
  SemanticM Unit
sAttributes property@(Property { attributes }) fl fe = traverse_ go attributes
  where
  go (Attribute _ (AttributeName span attributeName) (ALiteral _ value)) =
    case M.lookup attributeName fl of
      Just f -> f property value
      Nothing -> throwDiagnostic span $ "Attribute \""<> attributeName <> "\" does not exist"
  go (Attribute _ (AttributeName span attributeName) (AExpr _ value)) =
    case M.lookup attributeName fe of
      Just f -> f property value
      Nothing -> throwDiagnostic span $ "Attribute \""<> attributeName <> "\" does not exist"

sProgram :: Program -> SemanticM Unit
sProgram (Program { collections }) = do
  context <- ask
  _ <- L.foldM go context collections
  pure unit
  where
    go context collection = local (const context) (sCollection collection)

runSemantic :: String -> String -> Program -> Either Diagnostic Unit
runSemantic filepath source program = do
  let semantic = runReaderT (sProgram program) (emptyContext filepath source)
  runExcept semantic
