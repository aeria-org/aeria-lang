module Aeria.Semantic where

import Prelude

import Aeria.Diagnostic.Message (Diagnostic(..), DiagnosticInfo(..))
import Aeria.Diagnostic.Position (Span)
import Aeria.Semantic.Error (ExprError(..), PropertyError(..), SemanticError(..))
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionFilters, CollectionFiltersPresets, CollectionForm, CollectionGetters, CollectionIndexes, CollectionLayout, CollectionName(..), CollectionProperties, CollectionRequired, CollectionSearch(..), CollectionTable, CollectionTableMeta, Cond(..), Expr(..), FilterItem(..), FiltersPresetsItem(..), FormItem(..), Getter(..), IndexesItem(..), LayoutItem(..), LayoutItemComponent(..), Literal(..), Program(..), Property(..), PropertyName(..), PropertyType(..), Required(..), TableItem(..), TableMetaItem(..), Typ(..))
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

extendContext :: CollectionName -> CollectionProperties -> CollectionGetters -> Context -> Context
extendContext (CollectionName _ collectionName) properties getters (Context { collections, source, filepath }) =
  case M.lookup collectionName collections of
    Just collectionContext ->
      let collectionContext' = extendGetters collectionContext
          collectionContext'' = extendProperties collectionContext'
      in Context { collections: M.insert collectionName collectionContext'' collections, filepath, source }
    Nothing ->
      let collectionContext' = extendProperties (CollectionContext { getters: M.empty, properties: M.empty })
          collectionContext'' = extendGetters collectionContext'
        in Context { collections: M.insert collectionName collectionContext'' collections, filepath, source }
  where
    extendProperties :: CollectionContext -> CollectionContext
    extendProperties (CollectionContext { getters: gettersContext, properties: propertiesContext }) =
      let propertiesContext' = L.foldl go propertiesContext properties
        in CollectionContext { properties: propertiesContext', getters: gettersContext }
      where
        go pts property@(Property { name: (PropertyName _ name) }) = M.insert name property pts

    extendGetters :: CollectionContext -> CollectionContext
    extendGetters (CollectionContext { getters: gettersContext, properties: propertiesContext }) =
      let gettersContext' = L.foldl go gettersContext getters
        in CollectionContext { properties: propertiesContext, getters: gettersContext' }
      where
        go gts getter@(Getter { name: (PropertyName _ name) }) = M.insert name getter gts

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
  }) =
  local (extendContext name properties getters) $ do
    sProperties name properties
    sRequired name required
    sGetters name getters
    sTable name table
    sTableMeta name tableMeta
    sForm name form
    sFilters name filters
    sIndexes name indexes
    sLayout name layout
    sFiltersPresets name filtersPresets
    case search of
      Just search' -> sSearch name search'
      Nothing -> pure unit
    ask

sFiltersPresets :: CollectionName -> CollectionFiltersPresets -> SemanticM Unit
sFiltersPresets _ = traverse_ go
  where
  go filterPreset@(FiltersPresetsItem { span, filters }) = do
    when (isNothing filters) (throwDiagnostic span (FiltersPresetsError filterPreset))

sLayout :: CollectionName -> CollectionLayout -> SemanticM Unit
sLayout _ = traverse_ go
  where
    go (LayoutItem { component: Nothing }) = pure unit
    go layoutItem@(LayoutItem
      { component: (Just (LayoutItemComponent { span, name }))
      }) = when (isNothing name) (throwDiagnostic span (LayoutComponentError layoutItem))

sSearch :: CollectionName -> CollectionSearch -> SemanticM Unit
sSearch collectionName (CollectionSearch { indexes }) = sCheckIfPropertiesIsValid collectionName indexes

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
sIndexes collectionName collectionForm =
  let properties = map (\(IndexesItem _ propertyName) -> propertyName) collectionForm
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
  when (isJust (lookupProperty context collectionName name)) (throwDiagnostic span (DuplicateProperty name))

sProperties :: CollectionName -> CollectionProperties -> SemanticM Unit
sProperties collectionName = traverse_ (sProperty collectionName)

sProperty :: CollectionName -> Property -> SemanticM Unit
sProperty collectionName property@(Property { type_ }) =
  case type_ of
    PBoolean _ -> sBooleanProperty property
    PArray _ _ -> sArrayProperty collectionName property
    PObject _ _ -> sObjectProperty collectionName property
    PEnum _ -> sEnumProperty property
    PString _ -> sStringProperty property
    PFloat _ -> sNumberProperty property
    PInteger _ -> sNumberProperty property
    PRef _ (CollectionName _ "file") -> sFileProperty property
    PRef _ ref -> sRefProperty ref property

sBooleanProperty :: Property -> SemanticM Unit
sBooleanProperty = sAttributes'
  where
    sAttributes' property@(Property { span, attributes })
      | L.length attributes > 0 =
        throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectAttributes)
      | otherwise = pure unit

sArrayProperty :: CollectionName -> Property -> SemanticM Unit
sArrayProperty collectionName = sAttributes'
  where
    sAttributes' (Property { name, type_: (PArray span type'), attributes }) =
      case type' of
        PObject _ properties  -> traverse_ (sProperty collectionName) properties
        _                     -> sProperty collectionName (Property { span, type_: type', attributes, name })
    sAttributes' property@(Property { span }) =
      throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectType)

sObjectProperty :: CollectionName -> Property -> SemanticM Unit
sObjectProperty collectionName = sAttributes'
  where
    sAttributes' property@(Property { span, type_, attributes })
      | L.length attributes > 0 =
        throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectAttributes)
      | otherwise =
        case type_ of
          PObject _ properties -> traverse_ (sProperty collectionName) properties
          _ -> throwDiagnostic span (PropertyError property PropertyTypeDoesNotExpectType)

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
      throwDiagnostic (literalPos literal) (PropertyError property' (TypeMismatch [TArray] (typeOf literal)))

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
  when (type' `elem` accepts) (throwDiagnostic span (PropertyError property (TypeMismatch accepts type')))

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
