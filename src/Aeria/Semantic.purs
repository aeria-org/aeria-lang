module Aeria.Semantic where

import Prelude

import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionName(..), Expr(..), Getter(..), Getters, Ident, Literal(..), Macro(..), Program(..), Properties, Property(..), PropertyName, PropertyType(..), Required, RequiredProperty(..), Table, Typ(..))
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as L
import Data.Map.Internal (Map, empty, fromFoldable, insert, lookup) as M
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

type SemanticM a
  = ReaderT Context (Except SemanticError) a

data CollectionContext
  = CollectionContext
    { properties :: M.Map PropertyName Property
    , getters :: M.Map PropertyName Getter
    }

data Context
  = Context
    { collections :: M.Map CollectionName CollectionContext
    }

data PropertiesError
  = PTypeMismatch Typ Typ
  | PTypesMismatch (L.List Typ) Typ
  | PArrayTypeMismatch Typ Typ
  | PUndefinedAttribute AttributeName
  | PUndefinedReference CollectionName
  | PUndefinedProperty PropertyName
  | PPropertyTypeDoesNotExpectAttributes (L.List AttributeName)
  | PAttributeLiteralMustBe (L.List Ident)

derive instance genericPropertiesError :: Generic PropertiesError _

instance showPropertiesError :: Show PropertiesError where
  show = genericShow

data GetterError
  = InvalidLanguage Ident

derive instance genericGetterError :: Generic GetterError _

instance showGetterError :: Show GetterError where
  show = genericShow

data ExprError
  = ExpectedProperty

derive instance genericExprError :: Generic ExprError _

instance showExprError :: Show ExprError where
  show = genericShow

data TableError
  = TUndefinedProperty

derive instance genericTableError :: Generic TableError _

instance showTableError :: Show TableError where
  show = genericShow

data RequiredError
  = RUndefinedProperty

derive instance genericRequiredError :: Generic RequiredError _

instance showRequiredError :: Show RequiredError where
  show = genericShow

data SemanticError
  = PropertiesError Property PropertiesError
  | GetterError Getter GetterError
  | TableError PropertyName TableError
  | RequiredError PropertyName RequiredError
  | ExprError Expr ExprError
  | Unknown

derive instance genericSemanticError :: Generic SemanticError _

instance showSemanticError :: Show SemanticError where
  show = genericShow

emptyContext :: Context
emptyContext = Context { collections: M.empty }

lookupProperty :: Context -> CollectionName -> PropertyName -> Maybe Property
lookupProperty context collectionName propertyName = do
  let
    Context { collections } = context
  case M.lookup collectionName collections of
    Just (CollectionContext { properties }) -> M.lookup propertyName properties
    Nothing -> Nothing

propertyExists :: Context -> CollectionName -> PropertyName -> Maybe Unit
propertyExists context collectionName propertyName = do
  let
    Context { collections } = context
  case M.lookup collectionName collections of
    Nothing -> Nothing
    Just (CollectionContext { getters, properties }) -> case M.lookup propertyName properties of
      Just _ -> Just unit
      Nothing -> case M.lookup propertyName getters of
        Just _ -> Just unit
        Nothing -> Nothing

checkExpr :: CollectionName -> Expr -> SemanticM Unit
checkExpr collectionName = go
  where
  go expr@(EIn e1 e2) = checkBinaryExpr expr e1 e2
  go expr@(EEq e1 e2) = checkBinaryExpr expr e1 e2
  go expr@(ELt e1 e2) = checkBinaryExpr expr e1 e2
  go expr@(EGt e1 e2) = checkBinaryExpr expr e1 e2
  go expr@(ELte e1 e2) = checkBinaryExpr expr e1 e2
  go expr@(EGte e1 e2) = checkBinaryExpr expr e1 e2
  go (EOr e1 e2) = do
    checkExpr collectionName e1
    checkExpr collectionName e2
  go (EAnd e1 e2) = do
    checkExpr collectionName e1
    checkExpr collectionName e2
  go (EExists expr) = checkExists expr
  go (ENot expr) = checkExpr collectionName expr
  go (ELiteral _) = pure unit

  checkPropertyExists :: PropertyName -> SemanticM Unit
  checkPropertyExists propertyName = do
    context <- ask
    case lookupProperty context collectionName propertyName of
      Nothing -> throwError (RequiredError propertyName RUndefinedProperty)
      Just _ -> pure unit

  checkExists :: Expr -> SemanticM Unit
  checkExists expr = case expr of
    (ELiteral (LProperty propertyName)) -> checkPropertyExists propertyName
    _ -> throwError (ExprError expr ExpectedProperty)

  checkBinaryExpr :: Expr -> Expr -> Expr -> SemanticM Unit
  checkBinaryExpr expr e1 e2 = case e1 /\ e2 of
    (ELiteral (LProperty propertyName)) /\ _ -> do
      checkPropertyExists propertyName
      checkExpr collectionName e2
    _ /\ (ELiteral (LProperty propertyName)) -> do
      checkPropertyExists propertyName
      checkExpr collectionName e1
    _ /\ _ -> throwError (ExprError expr ExpectedProperty)

inferArray :: L.List Literal -> Maybe Typ
inferArray L.Nil = Nothing
inferArray (a : as) = go as (inferLiteral a)
  where
  go L.Nil t = Just t
  go (x : xs) t
    | inferLiteral x /= t = Nothing
    | otherwise = go xs t

inferLiteral :: Literal -> Typ
inferLiteral (LInteger _) = TInteger
inferLiteral (LFloat _) = TFloat
inferLiteral (LString _) = TString
inferLiteral (LBoolean _) = TBoolean
inferLiteral (LArray _) = TArray
inferLiteral (LProperty _) = TProperty

checkArrayType :: Typ -> Property -> Literal -> Either SemanticError Unit
checkArrayType expected property (LArray (l : ls)) = case inferArray (l : ls) of
  Just arrType -> when (arrType /= expected) (Left (PropertiesError property (PArrayTypeMismatch expected arrType)))
  Nothing -> Left (PropertiesError property (PArrayTypeMismatch expected (inferLiteral l)))

checkArrayType expected property literal = Left (PropertiesError property (PArrayTypeMismatch expected (inferLiteral literal)))

checkBoolean :: Property -> Literal -> Either SemanticError Unit
checkBoolean _ (LBoolean _) = Right unit
checkBoolean property literal = Left (PropertiesError property (PTypeMismatch TBoolean (inferLiteral literal)))

checkNumber :: Property -> Literal -> Either SemanticError Unit
checkNumber _ (LFloat _) = Right unit
checkNumber _ (LInteger _) = Right unit
checkNumber property literal = Left (PropertiesError property (PTypesMismatch (L.fromFoldable [ TFloat, TInteger ]) (inferLiteral literal)))

checkInteger :: Property -> Literal -> Either SemanticError Unit
checkInteger _ (LInteger _) = Right unit
checkInteger property literal = Left (PropertiesError property (PTypeMismatch TInteger (inferLiteral literal)))

-- collections
--
checkCollection :: Collection -> SemanticM Context
checkCollection = go
  where
  go ( Collection
      { name
    , properties
    , getters
    , table
    , required
    }
  ) =
    local (extendContext name properties getters)
      $ do
          checkProperties name properties
          checkGetters name getters
          checkTable name table
          checkRequired name required
          ask

  extendContext :: CollectionName -> Properties -> Getters -> Context -> Context
  extendContext collectionName properties getters ctx = updateCollectionCtx ctx
    where
    updateProperties :: CollectionContext -> CollectionContext
    updateProperties (CollectionContext { getters: gettersCtx, properties: propertiesCtx }) =
      let
        propertiesCtx' = L.foldl (\pts property@(Property { name }) -> M.insert name property pts) propertiesCtx properties
      in
        CollectionContext { properties: propertiesCtx', getters: gettersCtx }

    updateGetters :: CollectionContext -> CollectionContext
    updateGetters (CollectionContext { getters: gettersCtx, properties: propertiesCtx }) =
      let
        gettersCtx' = L.foldl (\gts getter@(Getter { name }) -> M.insert name getter gts) gettersCtx getters
      in
        CollectionContext { properties: propertiesCtx, getters: gettersCtx' }

    updateCollectionCtx :: Context -> Context
    updateCollectionCtx (Context { collections }) = case M.lookup collectionName collections of
      Nothing ->
        let
          collectionContext = updateProperties (CollectionContext { getters: M.empty, properties: M.empty })

          collectionContext' = updateGetters collectionContext
        in
          Context { collections: M.insert collectionName collectionContext' collections }
      Just collectionContext ->
        let
          collectionContext' = updateGetters collectionContext

          collectionContext'' = updateProperties collectionContext'
        in
          Context { collections: M.insert collectionName collectionContext'' collections }

checkRequired :: CollectionName -> Required -> SemanticM Unit
checkRequired collectionName =
  traverse_
    $ \(RequiredProperty propertyName condition) -> do
        context <- ask
        let
          property = lookupProperty context collectionName propertyName
        when (property == Nothing) (throwError (RequiredError propertyName RUndefinedProperty))
        case condition of
          Just expr -> checkExpr collectionName expr
          Nothing -> pure unit

checkTable :: CollectionName -> Table -> SemanticM Unit
checkTable collectionName = traverse_ go
  where
  go propertyName = do
    context <- ask
    case propertyExists context collectionName propertyName of
      Nothing -> throwError (TableError propertyName TUndefinedProperty)
      Just _ -> pure unit

checkGetters :: CollectionName -> Getters -> SemanticM Unit
checkGetters _ = traverse_ go
  where
  go getter@(Getter { macro: (Macro lang _) })
    | lang `elem` [ "js" ] = pure unit
    | otherwise = throwError (GetterError getter (InvalidLanguage lang))

checkProperties :: CollectionName -> Properties -> SemanticM Unit
checkProperties collectionName properties = do
  ctx <- ask
  traverse_
    ( \property -> do
        case checkProperty ctx collectionName property of
          Right _ -> pure unit
          Left err -> throwError err
    )
    properties

checkProperty :: Context -> CollectionName -> Property -> Either SemanticError Unit
checkProperty ctx collectionName property@(Property { type_ }) =
  let
    validate = mkPropertyValidate ctx collectionName type_
  in
    validate property

mkPropertyValidate :: Context -> CollectionName -> PropertyType -> Property -> Either SemanticError Unit
mkPropertyValidate ctx collectionName = case _ of
  PEnum -> checkEnumProperty
  PString -> checkStringProperty
  PFloat -> checkNumberProperty
  PInteger -> checkNumberProperty
  PBoolean -> checkBooleanProperty
  PRef (CollectionName "File") -> checkFileProperty
  PRef ref -> checkCollectionProperty ctx ref
  PObject _ -> checkObjectProperty ctx collectionName
  PArray _ -> checkArrayProperty ctx collectionName

checkBooleanProperty :: Property -> Either SemanticError Unit
checkBooleanProperty property@(Property { attributes })
  | L.length attributes > 0 =
    let
      attributeNames = map (\(Attribute attributeName _) -> attributeName) attributes
    in
      Left (PropertiesError property (PPropertyTypeDoesNotExpectAttributes attributeNames))
  | otherwise = pure unit

checkArrayProperty :: Context -> CollectionName -> Property -> Either SemanticError Unit
checkArrayProperty context collectionName (Property { name, type_, attributes }) = case type_ of
  PArray type_' -> case type_' of
    PObject properties -> traverse_ (checkProperty context collectionName) properties
    _ -> checkProperty context collectionName (Property { type_: type_', attributes, name })
  _ -> Left Unknown

checkObjectProperty :: Context -> CollectionName -> Property -> Either SemanticError Unit
checkObjectProperty context collectionName property@(Property { type_, attributes })
  | L.length attributes > 0 =
    let
      attributeNames = map (\(Attribute attributeName _) -> attributeName) attributes
    in
      Left (PropertiesError property (PPropertyTypeDoesNotExpectAttributes attributeNames))
  | otherwise = case type_ of
    PObject properties -> traverse_ (checkProperty context collectionName) properties
    _ -> Left Unknown

checkCollectionProperty :: Context -> CollectionName -> Property -> Either SemanticError Unit
checkCollectionProperty context ref property =
  let
    Context { collections } = context
  in
    case M.lookup ref collections of
      Nothing -> Left (PropertiesError property (PUndefinedReference ref))
      Just _ -> do
        mapAttributesLiteral property literalValidations exprValidations

  where
  literalValidations =
    M.fromFoldable
      [ "indexes" /\ checkArrayType'
      , "populate" /\ checkArrayType'
      , "inline" /\ checkBoolean
      ]

  exprValidations =
    M.fromFoldable
      ["constraints" /\ (\_ _ -> Right unit)
      ]

  checkArrayType' property' literal@(LArray values) = do
    checkArrayType TProperty property' literal
    traverse_ propertyExists' values
  checkArrayType' property' literal = Left (PropertiesError property' (PTypeMismatch TArray (inferLiteral literal)))

  propertyExists' (LProperty propertyName) = case propertyExists context ref propertyName of
    Nothing -> Left (PropertiesError property (PUndefinedProperty propertyName))
    Just _ -> pure unit
  propertyExists' literal = Left (PropertiesError property (PTypeMismatch TArray (inferLiteral literal)))

checkStringProperty :: Property -> Either SemanticError Unit
checkStringProperty property = mapAttributesLiteral property validations M.empty
  where
  validations :: M.Map String (Property -> Literal -> Either SemanticError Unit)
  validations =
    M.fromFoldable
      [ "minLength" /\ checkInteger
      , "maxLength" /\ checkInteger
      , "format" /\ checkFormat
      , "type" /\ checkType
      , "mask" /\ checkMask
      ]

  checkFormat :: Property -> Literal -> Either SemanticError Unit
  checkFormat property' (LString value)
    | elem value formatOptions = Right unit
    | otherwise = Left (PropertiesError property' (PAttributeLiteralMustBe (L.fromFoldable formatOptions)))
  checkFormat property' literal = Left (PropertiesError property' (PTypeMismatch TString received))
    where
    received = inferLiteral literal

  checkType :: Property -> Literal -> Either SemanticError Unit
  checkType property' (LString value)
    | elem value typeOptions = Right unit
    | otherwise = Left (PropertiesError property' (PAttributeLiteralMustBe (L.fromFoldable typeOptions)))
  checkType property' literal = Left (PropertiesError property' (PTypeMismatch TString received))
    where
    received = inferLiteral literal

  checkMask :: Property -> Literal -> Either SemanticError Unit
  checkMask property' array@(LArray _) = checkArrayType TString property' array
  checkMask _ (LString _) = Right unit
  checkMask property' literal = Left (PropertiesError property' (PTypesMismatch expected received))
    where
    received = inferLiteral literal
    expected = L.fromFoldable [ TString, TArray ]

  typeOptions :: Array String
  typeOptions = [ "text", "email", "password", "search", "time", "month" ]

  formatOptions :: Array String
  formatOptions = [ "date", "date-time" ]

checkNumberProperty :: Property -> Either SemanticError Unit
checkNumberProperty property = mapAttributesLiteral property validations M.empty
  where
  validations =
    M.fromFoldable
      [ "minimum" /\ checkNumber
      , "maximum" /\ checkNumber
      , "exclusiveMinimum" /\ checkNumber
      , "exclusiveMaximum" /\ checkNumber
      ]

checkFileProperty :: Property -> Either SemanticError Unit
checkFileProperty property = mapAttributesLiteral property validations M.empty
  where
  validations =
    M.fromFoldable
      [ "accept" /\ checkArrayType TString
      ]

checkEnumProperty :: Property -> Either SemanticError Unit
checkEnumProperty property = mapAttributesLiteral property validations M.empty
  where
  validations =
    M.fromFoldable
      [ "options" /\ checkArrayType TString
      ]

mapAttributesLiteral ::
  Property ->
  M.Map String (Property -> Literal -> Either SemanticError Unit) ->
  M.Map String (Property -> Expr -> Either SemanticError Unit) ->
  Either SemanticError Unit
mapAttributesLiteral property@(Property { attributes }) fl fe = traverse_ go attributes
  where
  go :: Attribute -> Either SemanticError Unit
  go (Attribute attribute@(AttributeName attributeName) (ALiteral value)) =
    case M.lookup attributeName fl of
      Just f -> f property value
      Nothing -> Left (PropertiesError property (PUndefinedAttribute attribute))
  go (Attribute attribute@(AttributeName attributeName) (AExpr value)) =
    case M.lookup attributeName fe of
      Just f -> f property value
      Nothing -> Left (PropertiesError property (PUndefinedAttribute attribute))

checkProgram :: Program -> SemanticM Unit
checkProgram (Program { collections }) = do
  context <- ask
  _ <- L.foldM go context collections
  pure unit
  where
  go context collection = local (\_ -> context) (checkCollection collection)

runSemantic :: Program -> Either SemanticError Unit
runSemantic program =
  let
    semantic = runReaderT (checkProgram program) emptyContext
  in
    runExcept semantic
