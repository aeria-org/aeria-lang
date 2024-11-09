module Aeria.Semantic where

import Aeria.Syntax.Tree
import Prelude

import Aeria.Diagnostic.Message (Diagnostic)
import Aeria.Semantic.Constraints (arrayAttributeNames, sArrayAttributes, sBooleanAttributes, sConstAttributes, sEnumAttributes, sFileAttributes, sNumberAttributes, sRefAttributes, sStringAttributes)
import Aeria.Semantic.Expr (sExpr)
import Aeria.Semantic.Internal (Context, SemanticM, collectionHasProperties, collectionHasProperty, emptyContext, extendContext, lookupCollection, throwDiagnostic)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask, local, runReaderT)
import Data.Array (elem)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List as L
import Data.Maybe (Maybe(..), isNothing)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

sCollection :: Collection -> SemanticM Context
sCollection collection@(Collection { security, functions }) = do
  sFunctions functions
  sSecurity functions security
  sDescription collection

sDescription :: Collection -> SemanticM Context
sDescription (Collection
  { name
  , properties
  , owned
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
  , formLayout
  , actions
  , individualActions
  , writable
  , immutable
  }) = do
  context <- ask >>= extendContext name properties
  local (const context) go
  where
    go = do
      sProperties name properties
      sOwned owned
      sRequired name required
      sTableLayout name tableLayout
      sLayout name layout
      sFiltersPresets filtersPresets
      sActions actions
      sIndividualActions individualActions
      sFormLayout formLayout
      sImmutable name immutable
      sPreferred name preferred
      collectionHasProperties name table
      collectionHasProperties name tableMeta
      collectionHasProperties name form
      collectionHasProperties name filters
      collectionHasProperties name indexes
      collectionHasProperties name writable
      sSearch name search
      ask

sOwned :: Maybe CollectionOwned -> SemanticM Unit
sOwned (Just (CollectionOwnedCustom span owned)) =
  case owned of
    "always" -> pure unit
    "on-write" -> pure unit
    _ -> throwDiagnostic span "Expected value \"always\", \"on-write\" or boolean"
sOwned _ = pure unit

sFiltersPresets :: CollectionFiltersPresets -> SemanticM Unit
sFiltersPresets = traverse_ go
  where
  go (FiltersPresetsItem { span, filters }) = do
    when (isNothing filters) (throwDiagnostic span "\"filters\" property in \"filtersPresets\" is required")

sFormLayout :: CollectionFormLayout -> SemanticM Unit
sFormLayout = traverse_ go
  where
  go (LayoutItem { component: Nothing }) = pure unit
  go (LayoutItem { component: (Just (LayoutItemComponent { span, name })) }) =
    when (isNothing name) (throwDiagnostic span "\"name\" property in \"formLayout\" is required")

sIndividualActions :: CollectionIndividualActions -> SemanticM Unit
sIndividualActions = traverse_ go
  where
  go (ActionItem { span, label }) =
    when (isNothing label) (throwDiagnostic span "\"label\" property in \"individualActions\" is required")

sActions :: CollectionActions -> SemanticM Unit
sActions = traverse_ go
  where
  go (ActionItem { span, label }) =
    when (isNothing label) (throwDiagnostic span "\"label\" property in \"actions\" is required")

sTableLayout :: CollectionName -> CollectionTableLayout -> SemanticM Unit
sTableLayout collectionName = traverse_ go
  where
  go (TableLayoutItem { button }) =
    case button of
      Just (Right cond) -> sCond collectionName cond
      _ -> pure unit

sLayout :: CollectionName -> Maybe CollectionLayout -> SemanticM Unit
sLayout _ Nothing = pure unit
sLayout collectionName (Just (CollectionLayout {span, name, options})) = do
  when (not (name `elem` ["grid", "tabular", "list"])) (throwDiagnostic span "Invalid layout name")
  case options of
    Just (LayoutOptions { title, badge, picture, information, active }) -> do
      let properties = L.fromFoldable [title, badge, picture, information, active]
      collectionHasProperties collectionName (L.catMaybes properties)
    Nothing -> pure unit

sSearch :: CollectionName -> Maybe CollectionSearch -> SemanticM Unit
sSearch collectionName (Just (CollectionSearch { indexes })) =
  collectionHasProperties collectionName indexes
sSearch _ _ = pure unit

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
      sActions actions
      sIndividualActions individualActions
      sFiltersPresets filtersPresets
      sLayout collectionName layout
      sFormLayout formLayout
      collectionHasProperties collectionName filters
      collectionHasProperties collectionName tableMeta
      collectionHasProperties collectionName table
      collectionHasProperties collectionName form
      sTableLayout collectionName tableLayout

sSecurity :: CollectionFunctions -> CollectionSecurity -> SemanticM Unit
sSecurity functions = traverse_ go
  where
  go (SecurityItem {span, functionName, rateLimiting, logging}) = do
    sFunctionName span functionName
    sRateLimiting rateLimiting
    sLogging logging

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

sRequired :: CollectionName -> Maybe CollectionRequired -> SemanticM Unit
sRequired _ Nothing = pure unit
sRequired collectionName@(CollectionName _ collectionName') (Just required) = traverse_ go required
  where
    go (Required _ propertyName@(PropertyName span name) cond) = do
      context <- ask
      when (isNothing (collectionHasProperty context collectionName propertyName))
        (throwDiagnostic span $ "Property \"" <> name <> "\" does not exist on collection \"" <> collectionName' <> "\"")
      case cond of
        Just cond' -> sCond collectionName cond'
        Nothing -> pure unit

sFunctions :: CollectionFunctions -> SemanticM Unit
sFunctions collectionFunctions = traverse_ (\(FunctionItem { expose }) -> go expose) collectionFunctions
  where
    go (Just (Attribute span attributeName value)) =
      case getName attributeName, value of
        "expose", (ALiteral _ (LArray _ _)) -> pure unit
        "expose", (ALiteral _ (LBoolean _ _)) -> pure unit
        "expose", (ALiteral _ (LString _ _)) -> pure unit
        name, _ -> throwDiagnostic span $ "Attribute \"" <> name <> "\" does not exist on functions"
    go Nothing = pure unit

sImmutable :: CollectionName -> Maybe CollectionImmutable -> SemanticM Unit
sImmutable collectionName (Just (CollectionImmutableList immutable)) =
  collectionHasProperties collectionName immutable
sImmutable _ (Just (CollectionImmutableBool _)) = pure unit
sImmutable _ Nothing = pure unit

sProperties :: CollectionName -> CollectionProperties -> SemanticM Unit
sProperties collectionName = traverse_ (sProperty collectionName)

sProperty :: CollectionName -> Property -> SemanticM Unit
sProperty collectionName property@(Property { type_ }) =
  case type_ of
    PNum _ -> sNumberAttributes collectionName property
    PEnum _ -> sEnumAttributes collectionName property
    PConst _ -> sConstAttributes collectionName property
    PString _ -> sStringAttributes collectionName property
    PInteger _ -> sNumberAttributes collectionName property
    PRef _ (CollectionName _ "File") -> sFileAttributes collectionName property
    PRef _ ref -> sRefProperty ref property
    PBoolean _ -> sBooleanAttributes collectionName property
    PArray _ _ -> sArrayProperty collectionName property
    PObject _ _ _ _ -> sObjectProperty collectionName property

sRefProperty :: CollectionName -> Property -> SemanticM Unit
sRefProperty ref property@(Property { span }) = do
  context <- ask
  when (isNothing (lookupCollection context ref))
    (throwDiagnostic span ("Cannot find collection \"" <> getName ref <> "\""))
  sRefAttributes ref property

sObjectProperty :: CollectionName -> Property -> SemanticM Unit
sObjectProperty = go 0
  where
    go idx collectionName (Property { span, type_ }) = do
      case type_ of
        PObject _ required properties _ -> do
          let collectionName' =  (CollectionName span (getName collectionName <> (show idx)))
          context <- ask >>= extendContext collectionName' properties
          local (const context) $ do
            sRequired collectionName' required
            traverse_ (sProperty collectionName') properties
        _ -> unsafePerformEffect (throw "unresearchable")

sArrayProperty :: CollectionName -> Property -> SemanticM Unit
sArrayProperty collectionName property@(Property { type_: (PArray span' type'), attributes, name }) = do
  let typeAttributes = getTypeAttributes attributes
  sArrayAttributes collectionName property
  sProperty collectionName (Property { span: span', type_: type', attributes: typeAttributes, name })
  where
    getTypeAttributes :: L.List Attribute -> L.List Attribute
    getTypeAttributes = L.filter (\(Attribute _ attributeName _) -> not (getName attributeName `A.elem` arrayAttributeNames))
sArrayProperty _ _ = unsafePerformEffect (throw "unresearchable")

sCond :: CollectionName -> Cond -> SemanticM Unit
sCond collectionName (Cond span expr) = do
  context <- ask
  case sExpr context collectionName expr of
    Left err -> throwDiagnostic span err
    Right _ -> pure unit

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


