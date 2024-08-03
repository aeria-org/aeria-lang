module Aeria.Semantic.Internal where

import Prelude

import Aeria.Diagnostic.Message (Diagnostic(..))
import Aeria.Diagnostic.Position (Span)
import Aeria.Syntax.Tree (CollectionGetters, CollectionName, CollectionProperties, Getter(..), Property(..), PropertyName, getName, getSpan)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (Except)
import Control.Monad.Reader (class MonadReader, ReaderT, ask)
import Data.Array as A
import Data.Foldable (traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))

type SemanticM a = ReaderT Context (Except Diagnostic) a

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

extendContext
  :: forall m. MonadReader Context m
  => MonadError Diagnostic m
  => CollectionName
  -> CollectionProperties
  -> CollectionGetters
  -> Context
  -> m Context
extendContext collectionName properties getters context@(Context { collections, source, filepath }) = do
  let collectionName' = getName collectionName
  case M.lookup collectionName' collections of
    Just collectionContext -> do
      collectionContext' <- extendGetters collectionContext
      collectionContext'' <- extendProperties collectionContext'
      pure $ Context { collections: M.insert collectionName' collectionContext'' collections, filepath, source }
    Nothing -> do
      collectionContext' <- extendProperties (CollectionContext { getters: M.empty, properties: M.empty })
      collectionContext'' <- extendGetters collectionContext'
      pure $ Context { collections: M.insert collectionName' collectionContext'' collections, filepath, source }
  where
    extendProperties
      :: CollectionContext
      -> m CollectionContext
    extendProperties (CollectionContext { getters: gettersCtx, properties: propertiesCtx }) = do
      propertiesCtx' <- L.foldM go propertiesCtx properties
      pure $ CollectionContext { properties: propertiesCtx', getters: gettersCtx }
      where
        go ctx property@(Property { name }) = do
          let name' = getName name
          case M.lookup name' ctx of
            Nothing -> pure (M.insert name' property ctx)
            Just _ -> do
              let diagnostic = makeDiagnostic context (getSpan name) $ "Property \"" <> name' <> "\" already defined"
              throwError diagnostic

    extendGetters
      :: CollectionContext
      -> m CollectionContext
    extendGetters (CollectionContext { getters: gettersCtx, properties: propertiesCtx }) = do
      gettersCtx' <- L.foldM go gettersCtx getters
      pure $ CollectionContext { properties: propertiesCtx, getters: gettersCtx' }
      where
        go ctx getter@(Getter { name }) = do
          let name' = getName name
          case M.lookup name' ctx of
            Nothing -> pure (M.insert name' getter ctx)
            Just _ -> do
              let diagnostic = makeDiagnostic context (getSpan name) $ "Getter \"" <> name' <> "\" already defined"
              throwError diagnostic

throwDiagnostic
  :: forall m. (MonadReader Context m)
  => MonadError Diagnostic m
  => Span
  -> String
  -> m Unit
throwDiagnostic span semanticError = do
  context <- ask
  let diagnostic = makeDiagnostic context span semanticError
  throwError diagnostic

makeDiagnostic :: Context -> Span -> String -> Diagnostic
makeDiagnostic (Context { filepath, source }) span info =
  Diagnostic
    { filepath
    , span
    , source
    , info
    }

lookupCollection :: Context -> CollectionName -> Maybe CollectionContext
lookupCollection (Context { collections }) collectionName = M.lookup (getName collectionName) collections

lookupProperty :: Context -> CollectionName -> PropertyName -> Maybe Property
lookupProperty (Context { collections }) collectionName propertyName = do
  case M.lookup (getName collectionName) collections of
    Just (CollectionContext { properties }) -> M.lookup (getName propertyName) properties
    Nothing -> Nothing

lookupGetter :: Context -> CollectionName -> PropertyName -> Maybe Getter
lookupGetter (Context { collections }) collectionName propertyName = do
  case M.lookup (getName collectionName) collections of
    Just (CollectionContext { getters }) -> M.lookup (getName propertyName) getters
    Nothing -> Nothing

aeriaProperties :: Array String
aeriaProperties =
  ["_id"
  ]

collectionHasPropertyOrGetter :: Context -> CollectionName -> PropertyName -> Maybe Unit
collectionHasPropertyOrGetter context collectionName propertyName =
  when (getName propertyName `A.elem` aeriaProperties) do
    case lookupGetter context collectionName propertyName of
      Nothing ->
        case lookupProperty context collectionName propertyName of
          Nothing -> Nothing
          Just _ -> Just unit
      Just _ -> Just unit

collectionHasProperty :: Context -> CollectionName -> PropertyName -> Maybe Unit
collectionHasProperty context collectionName propertyName = do
  when (not (getName propertyName `A.elem` aeriaProperties))
    case lookupProperty context collectionName propertyName of
      Nothing -> Nothing
      Just _ -> Just unit

collectionHasProperties :: CollectionName -> L.List PropertyName -> SemanticM Unit
collectionHasProperties collectionName = traverse_ \propertyName -> do
  context <- ask
  case collectionHasPropertyOrGetter context collectionName propertyName of
    Just _  -> pure unit
    Nothing ->
      throwDiagnostic (getSpan propertyName)
        ("Property \"" <> getName propertyName <> "\" does not exist on collection \"" <> getName collectionName <> "\"")
