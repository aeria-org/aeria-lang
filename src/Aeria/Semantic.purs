module Aeria.Semantic where

import Prelude

import Aeria.Diagnostic.Message (Diagnostic(..), DiagnosticInfo(..))
import Aeria.Diagnostic.Position (Span)
import Aeria.Semantic.Error (ExprError(..), PropertyError(..), SemanticError(..))
import Aeria.Syntax.Tree (ActionItem(..), Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionActions, CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionFormLayout, CollectionFunctions, CollectionGetters, CollectionImmutable(..), CollectionIndexes, CollectionIndividualActions, CollectionLayout, CollectionName(..), CollectionPreferred, CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionSecurity, CollectionTable, CollectionTableLayout, CollectionTableMeta, CollectionWritable, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), FunctionItem(..), FunctionName(..), Getter(..), ImmutableItem(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), PreferredItem(..), Program(..), Property(..), PropertyName(..), PropertyType(..), Required(..), SecurityItem(..), SecurityLogging(..), SecurityRateLimiting(..), TableItem(..), TableLayoutItem(..), TableMetaItem(..), Typ(..), WritableItem(..))
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
        go pts property@(Property { name: pn@(PropertyName span name) }) = do
          context <- ask
          case M.lookup name pts of
            Nothing -> pure $ M.insert name property pts
            Just _ -> do
              let diagnostic = makeDiagnostic context span (PropertyIsAlreadyInUse pn)
              throwError diagnostic


    extendGetters :: CollectionContext -> SemanticM CollectionContext
    extendGetters (CollectionContext { getters: gettersContext, properties: propertiesContext }) = do
      gettersContext' <- L.foldM go gettersContext getters
      pure $ CollectionContext { properties: propertiesContext, getters: gettersContext' }
      where
        go gts getter@(Getter { name: pn@(PropertyName span name) }) = do
          context <- ask
          case M.lookup name gts of
            Nothing -> pure $ M.insert name getter gts
            Just _ -> do
              let diagnostic = makeDiagnostic context span (PropertyIsAlreadyInUse pn)
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

collectionHasProperty :: Context -> CollectionName -> PropertyName -> Maybe Unit
collectionHasProperty context collectionName propertyName =
  case lookupGetter context collectionName propertyName of
    Nothing ->
      case lookupProperty context collectionName propertyName of
        Nothing -> Nothing
        Just _ -> Just unit
    Just _ -> Just unit

makeDiagnostic :: Context -> Span -> SemanticError -> Diagnostic
makeDiagnostic (Context { filepath, source }) span semanticError =
  Diagnostic
    { filepath
    , span
    , source
    , info: DiagnosticSemanticError semanticError
    }

throwDiagnostic :: Span -> SemanticError -> SemanticM Unit
throwDiagnostic span semanticError = do
  context <- ask
  let diagnostic = makeDiagnostic context span semanticError
  throwError diagnostic

sExpr :: Context -> CollectionName -> Expr -> Either ExprError Unit
sExpr context collectionName expr =
  case expr of
    EIn lft rgt   -> sBinaryExpr lft rgt
    EEq lft rgt   -> sBinaryExpr lft rgt
    ELt lft rgt   -> sBinaryExpr lft rgt
    EGt lft rgt   -> sBinaryExpr lft rgt
    ELte lft rgt  -> sBinaryExpr lft rgt
    EGte lft rgt  -> sBinaryExpr lft rgt
    EOr lft rgt   -> sBinaryExpr lft rgt
    EAnd lft rgt  -> sBinaryExpr lft rgt
    EExists expr' -> sExists expr'
    ENot expr'    -> sExpr context collectionName expr'
    ELiteral _    -> pure unit
  where
    sBinaryExpr :: Expr -> Expr -> Either ExprError Unit
    sBinaryExpr lft rgt =
      case lft /\ rgt of
        (ELiteral (LProperty _ propertyName)) /\ _ -> do
          isStrictProperty propertyName
          sExpr context collectionName rgt
        _ /\ (ELiteral (LProperty _ propertyName)) -> do
          isStrictProperty propertyName
          sExpr context collectionName lft
        _ /\ _ -> Left ExpectedProperty

    sExists :: Expr -> Either ExprError Unit
    sExists (ELiteral (LProperty _ propertyName)) = isStrictProperty propertyName
    sExists _ = Left ExpectedProperty

    isStrictProperty :: PropertyName -> Either ExprError Unit
    isStrictProperty propertyName =
      when (isNothing (lookupProperty context collectionName propertyName)) (Left $ NotProperty propertyName)

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
  go filterPreset@(FiltersPresetsItem { span, filters }) = do
    when (isNothing filters) (throwDiagnostic span (FiltersPresetsError filterPreset))

sFormLayout :: CollectionName -> CollectionFormLayout -> SemanticM Unit
sFormLayout _ = traverse_ go
  where
  go (LayoutItem { component: Nothing }) = pure unit
  go layoutItem@(LayoutItem
    { component: (Just (LayoutItemComponent { span, name }))
    }) = when (isNothing name) (throwDiagnostic span (LayoutComponentError layoutItem))

sIndividualActions :: CollectionName -> CollectionIndividualActions -> SemanticM Unit
sIndividualActions _ = traverse_ go
  where
  go action@(ActionItem
    { span
    , label
    }) = do
      when (isNothing label) (throwDiagnostic span (ActionError action))

sActions :: CollectionName -> CollectionActions -> SemanticM Unit
sActions _ = traverse_ go
  where
  go action@(ActionItem
    { span
    , label
    }) = do
      when (isNothing label) (throwDiagnostic span (ActionError action))

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
            Left err ->  throwDiagnostic span (ExprError cond err)
            Right _ -> pure unit
        _ -> pure unit

sLayout :: CollectionName -> CollectionLayout -> SemanticM Unit
sLayout _ = traverse_ go
  where
  go (LayoutItem { component: Nothing }) = pure unit
  go layoutItem@(LayoutItem
    { component: (Just (LayoutItemComponent { span, name }))
    }) = when (isNothing name) (throwDiagnostic span (LayoutComponentError layoutItem))

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
sSearch collectionName (CollectionSearch { indexes }) = sCheckIfPropertiesIsValid collectionName indexes

sSecurity :: CollectionFunctions -> CollectionSecurity -> SemanticM Unit
sSecurity functions = traverse_ go
  where
  go (SecurityItem {span, functionName, rateLimiting, logging}) = do
    sFunctionName span functionName
    sRateLimiting rateLimiting
    sLogging logging
    pure unit

  sFunctionName span functionName@(FunctionName _ name) =
    case L.find (\(FunctionItem _ (FunctionName _ function) _ _) -> function == name) functions of
      Just _ -> pure unit
      Nothing -> throwDiagnostic span (UndefinedFunction functionName)

  sRateLimiting (Just (SecurityRateLimiting {span, strategy})) =
    case strategy of
      Just strategy' ->
        if strategy' `elem` ["tenant", "ip"]
        then pure unit
        else throwDiagnostic span (UndefinedStrategy strategy')
      Nothing -> pure unit
  sRateLimiting _ = pure unit

  sLogging (Just (SecurityLogging { span, strategy: (Just strategy) })) =
    if strategy `elem` ["tenant", "ip"]
      then pure unit
      else throwDiagnostic span (UndefinedStrategy strategy)
  sLogging _ = pure unit

sRequired :: CollectionName -> CollectionRequired -> SemanticM Unit
sRequired collectionName = traverse_ go
  where
    go (Required _ propertyName@(PropertyName span _) cond) = do
      context <- ask
      when (isNothing (lookupProperty context collectionName propertyName)) (throwDiagnostic span (UndefinedProperty propertyName))
      sCond context cond

    sCond context (Just (Cond span expr)) =
      case sExpr context collectionName expr of
        Right unit -> pure unit
        Left exprError -> throwDiagnostic span (ExprError expr exprError)
    sCond _ Nothing = pure unit

sTable :: CollectionName -> CollectionTable -> SemanticM Unit
sTable collectionName collectionTable =
  let properties = map (\(TableItem _ propertyName) -> propertyName) collectionTable
    in sCheckIfPropertiesIsValid collectionName properties

sTableMeta :: CollectionName -> CollectionTableMeta -> SemanticM Unit
sTableMeta collectionName collectionTable =
  let properties = map (\(TableMetaItem _ propertyName) -> propertyName) collectionTable
    in sCheckIfPropertiesIsValid collectionName properties

sFilters :: CollectionName -> CollectionFilters -> SemanticM Unit
sFilters collectionName collectionFilters =
  let properties = map (\(FilterItem _ propertyName) -> propertyName) collectionFilters
    in sCheckIfPropertiesIsValid collectionName properties

sForm :: CollectionName -> CollectionForm -> SemanticM Unit
sForm collectionName collectionForm =
  let properties = map (\(FormItem _ propertyName) -> propertyName) collectionForm
    in sCheckIfPropertiesIsValid collectionName properties

sIndexes :: CollectionName -> CollectionIndexes -> SemanticM Unit
sIndexes collectionName collectionIndexes =
  let properties = map (\(IndexesItem _ propertyName) -> propertyName) collectionIndexes
    in sCheckIfPropertiesIsValid collectionName properties

sWritable :: CollectionName -> CollectionWritable -> SemanticM Unit
sWritable collectionName collectionWritable =
  let properties = map (\(WritableItem _ propertyName) -> propertyName) collectionWritable
    in sCheckIfPropertiesIsValid collectionName properties

-- sFunctions :: CollectionName -> CollectionFunctions -> SemanticM Unit
-- sFunctions collectionName collectionFunctions =
--   let properties = map (\(FunctionItem _ propertyName) -> propertyName) collectionFunctions
--     in sCheckIfPropertiesIsValid collectionName properties

sImmutable :: CollectionName -> Maybe CollectionImmutable -> SemanticM Unit
sImmutable _ Nothing = pure unit
sImmutable _ (Just (CollectionImmutableBool _)) = pure unit
sImmutable collectionName (Just (CollectionImmutableList immutable)) =
  let properties = map (\(ImmutableItem _ propertyName) -> propertyName) immutable
    in sCheckIfPropertiesIsValid collectionName properties

sCheckIfPropertiesIsValid :: CollectionName -> L.List PropertyName -> SemanticM Unit
sCheckIfPropertiesIsValid collectionName = traverse_ \propertyName@(PropertyName span _) -> do
  context <- ask
  case collectionHasProperty context collectionName propertyName of
    Just _  -> pure unit
    Nothing -> throwDiagnostic span (UndefinedProperty propertyName)

sGetters :: CollectionName -> CollectionGetters -> SemanticM Unit
sGetters collectionName = traverse_ \(Getter { name: name@(PropertyName span _) }) -> do
  context <- ask
  when (isJust (lookupProperty context collectionName name)) (throwDiagnostic span (PropertyIsAlreadyInUse name))

sProperties :: CollectionName -> CollectionProperties -> SemanticM Unit
sProperties collectionName = traverse_ (sProperty collectionName)

sProperty :: CollectionName -> Property -> SemanticM Unit
sProperty collectionName property@(Property { type_ }) =
  case type_ of
    PBoolean _ -> sBooleanProperty property
    PArray _ _ -> sArrayProperty collectionName property
    PObject _ _ _ -> sObjectProperty collectionName property
    PEnum _ -> sEnumProperty property
    PString _ -> sStringProperty property
    PFloat _ -> sNumberProperty property
    PInteger _ -> sNumberProperty property
    PRef _ (CollectionName _ "file") -> sFileProperty property
    PRef _ ref -> sRefProperty ref property

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
      let arrayAttributes = L.filter (\(Attribute _ (AttributeName _ attributeName) _) -> attributeName == "default") attributes
      let typeAttributes = L.filter (\(Attribute _ (AttributeName _ attributeName) _) -> attributeName /= "default") attributes
      sAttributes (Property { span, name, type_: (PArray span' type'), attributes: arrayAttributes }) literalAttributes M.empty
      case type' of
        object@(PObject _ _ _)  -> sObjectProperty collectionName (Property { span, type_: object, attributes: typeAttributes, name })
        _                       -> sProperty collectionName (Property { span, type_: type', attributes: typeAttributes, name })
    sAttributes' property@(Property { span }) =
      throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectType)

    literalAttributes =
      M.fromFoldable
        [ "default" /\ sType [TArray]
        ]

sObjectProperty :: CollectionName -> Property -> SemanticM Unit
sObjectProperty (CollectionName _ collectionName) = sAttributes' 0
  where
    sAttributes' idx property@(Property { span, type_ }) = do
      sAttributes property literalAttributes M.empty
      case type_ of
        PObject _ required properties -> do
          let objectName =  (CollectionName span (collectionName <> (show idx)))
          context <- ask >>= extendContext objectName properties L.Nil
          local (const context) $ do
            sRequired objectName required
            traverse_ (sProperty objectName) properties
        _ -> throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectType)

    literalAttributes =
      M.fromFoldable
        [ --"default" /\ sType []
        ]

sRefProperty :: CollectionName -> Property -> SemanticM Unit
sRefProperty collectionName@(CollectionName span _) property = do
  context <- ask
  when (isNothing (lookupCollection context collectionName))
    (throwDiagnostic span (PropertyError property (UndefinedReference collectionName)))
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
      traverse_ collectionHasProperty' values
    sArrayType' property' literal =
      throwDiagnostic (literalPos literal) (PropertyError property' (TypeMismatch [] (typeOf literal)))

    collectionHasProperty' (LProperty span' propertyName) = do
      context <- ask
      case collectionHasProperty context collectionName propertyName of
        Nothing ->
          throwDiagnostic span' (UndefinedProperty propertyName)
        Just _ -> pure unit
    collectionHasProperty' literal =
      throwDiagnostic (literalPos literal) (PropertyError property (TypeMismatch [TArray] (typeOf literal)))

    sConstraints _property' expr = do
      context <- ask
      case sExpr context collectionName expr of
        Right unit -> pure unit
        Left exprError -> throwDiagnostic span (ExprError expr exprError)

sFileProperty :: Property -> SemanticM Unit
sFileProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "accept" /\ sArrayType TString
      ]

sType :: Array Typ -> Property -> Literal -> SemanticM Unit
sType accepts property literal = do
  let type' = typeOf literal
  let span = literalPos literal
  when (not (type' `elem` accepts)) (throwDiagnostic span (PropertyError property (TypeMismatch accepts type')))

sArrayType :: Typ -> Property -> Literal -> SemanticM Unit
sArrayType expected property (LArray _ arr@(l:_)) =
  case typeOfArray arr of
    Just arrType ->
      when (arrType /= expected) $ throwDiagnostic (literalPos l) (PropertyError property (ArrayTypeMismatch expected arrType))
    Nothing ->
      throwDiagnostic (literalPos l) (PropertyError property (ArrayTypeMismatch expected (typeOf l)))
sArrayType expected property literal =
  throwDiagnostic (literalPos literal) (PropertyError property (ArrayTypeMismatch expected (typeOf literal)))

sEnumProperty :: Property -> SemanticM Unit
sEnumProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "options" /\ sArrayType TString
      , "default" /\ sType [TString]
      ]

sNumberProperty :: Property -> SemanticM Unit
sNumberProperty property = sAttributes property literalAttributes M.empty
  where
  literalAttributes =
    M.fromFoldable
      [ "minimum" /\ sType [TFloat, TInteger]
      , "maximum" /\ sType [TFloat, TInteger]
      , "exclusiveMinimum" /\ sType [TFloat, TInteger]
      , "exclusiveMaximum" /\ sType [TFloat, TInteger]
      , "default" /\ sType [TInteger, TFloat]
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
typeOf (LFloat _ _) = TFloat
typeOf (LString _ _) = TString
typeOf (LBoolean _ _) = TBoolean
typeOf (LArray _ _) = TArray
typeOf (LProperty _ _) = TProperty

literalPos :: Literal -> Span
literalPos (LInteger span _) = span
literalPos (LFloat span _) = span
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
      , "type" /\ checkType
      , "mask" /\ checkMask
      , "default" /\ sType [TString]
      ]

  checkFormat :: Property -> Literal -> SemanticM Unit
  checkFormat property' (LString span value)
    | elem value formatOptions = pure unit
    | otherwise = throwDiagnostic span (PropertyError property' (AttributeLiteralMustBe (L.fromFoldable formatOptions)))
  checkFormat property' literal = throwDiagnostic (literalPos literal) (PropertyError property' (TypeMismatch [TString] received))
    where
    received = typeOf literal

  checkType :: Property -> Literal -> SemanticM Unit
  checkType property' (LString span value)
    | elem value typeOptions = pure unit
    | otherwise = throwDiagnostic span (PropertyError property' (AttributeLiteralMustBe (L.fromFoldable typeOptions)))
  checkType property' literal = throwDiagnostic (literalPos literal) (PropertyError property' (TypeMismatch [TString] received))
    where
    received = typeOf literal

  checkMask :: Property -> Literal -> SemanticM Unit
  checkMask property' array@(LArray _ _) = sArrayType TString property' array
  checkMask _ (LString _ _) = pure unit
  checkMask property' literal = throwDiagnostic (literalPos literal) (PropertyError property' (TypeMismatch expected  received))
    where
    received = typeOf literal
    expected = [TString, TArray]

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
  go (Attribute _ attribute@(AttributeName span attributeName) (ALiteral _ value)) =
    case M.lookup attributeName fl of
      Just f -> f property value
      Nothing -> throwDiagnostic span (PropertyError property (UndefinedAttribute attribute))
  go (Attribute _ attribute@(AttributeName span attributeName) (AExpr _ value)) =
    case M.lookup attributeName fe of
      Just f -> f property value
      Nothing -> throwDiagnostic span (PropertyError property (UndefinedAttribute attribute))

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
