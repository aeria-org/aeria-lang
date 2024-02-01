module Aeria.Semantic.Monad
  ( PropertyError(..)
  , SemanticError(..)
  , runSemantic
  )
  where

import Prelude

import Aeria.Environment (Environment, empty, extend, lookup)
import Aeria.Syntax.Tree (Attribute(..), Collection(..), Expr(..), Getter(..), Getters, Macro(..), Name(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Required, RequiredProperty(..), Table, Typ(..), Value(..))
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.List ((:))
import Data.List as L
import Data.Map.Internal (Map, lookup, fromFoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

type SemanticM a = ReaderT Context (Except SemanticError) a

data Context = Context
  { properties :: Environment Property
  , getters :: Environment Getter
  }

data PropertyError
  = InvalidAttribute Name
  | AttributeValueMustBe (L.List String)
  | UnexpectedAttributes
  | TypeMismatch Typ Typ
  | TypesMismatch (L.List Typ) Typ
  | ArrayTypeMismatch Typ Typ

derive instance genericPropertyError :: Generic PropertyError _

instance showPropertyError :: Show PropertyError where
  show = genericShow

data SemanticError
  = PropertyError Property PropertyError
  | UndefinedProperty PropertyName
  | InvalidMacroGetter Getter
  | UndefinedGetter PropertyName
  | ExpectedPropertyExpr Expr
  | Unknown

derive instance genericSemanticError :: Generic SemanticError _

instance showSemanticError :: Show SemanticError where
  show = genericShow

emptyContext :: Context
emptyContext = Context { properties: empty, getters: empty }

lookupProperty :: PropertyName -> SemanticM (Maybe Property)
lookupProperty (PropertyName name) = do
  Context { properties } <- ask
  pure (lookup name properties)

lookupGetter :: PropertyName -> SemanticM (Maybe Getter)
lookupGetter (PropertyName name) = do
  Context { getters } <- ask
  pure (lookup name getters)

checkExpr :: Expr -> SemanticM Unit
checkExpr = go
  where
    go expr@(EIn e1 e2) = checkBinaryExpr expr e1 e2
    go expr@(EEq e1 e2) = checkBinaryExpr expr e1 e2
    go expr@(ELt e1 e2) = checkBinaryExpr expr e1 e2
    go expr@(EGt e1 e2) = checkBinaryExpr expr e1 e2
    go expr@(ELte e1 e2) = checkBinaryExpr expr e1 e2
    go expr@(EGte e1 e2) = checkBinaryExpr expr e1 e2
    go (EOr e1 e2) = do
      checkExpr e1
      checkExpr e2
    go (EAnd e1 e2) = do
      checkExpr e1
      checkExpr e2
    go (EExists expr) = checkExists expr
    go (ENot expr) = checkExpr expr
    go (EValue _) = pure unit

    checkPropertyExists :: Name -> SemanticM Unit
    checkPropertyExists (Name name) = do
      property <- lookupProperty (PropertyName name)
      when (property == Nothing) (throwError (UndefinedProperty (PropertyName name)))

    checkExists :: Expr -> SemanticM Unit
    checkExists expr =
      case expr of
        (EValue (VProperty name)) -> checkPropertyExists name
        _ -> throwError (ExpectedPropertyExpr expr)

    checkBinaryExpr :: Expr -> Expr -> Expr -> SemanticM Unit
    checkBinaryExpr expr e1 e2 =
      case e1 /\ e2 of
        (EValue (VProperty name)) /\ _  -> do
          checkPropertyExists name
          checkExpr e2
        _ /\ (EValue (VProperty name)) -> do
          checkPropertyExists name
          checkExpr e1
        _ /\ _ -> throwError (ExpectedPropertyExpr expr)

arrayType :: L.List Value -> Maybe Typ
arrayType L.Nil = Nothing
arrayType (a:as) = go as (valueType a)
  where
    go L.Nil t = Just t
    go (x:xs) t
      | valueType x /= t = Nothing
      | otherwise = go xs t

valueType :: Value -> Typ
valueType (VInteger _) = TInteger
valueType (VFloat _) = TFloat
valueType (VString _) = TString
valueType (VBoolean _) = TBoolean
valueType (VArray _) = TArray
valueType (VProperty _) = TProperty

checkArrayType :: Typ -> Property -> Value -> Either SemanticError Unit
checkArrayType expected property (VArray (v:vs)) =
  case arrayType (v:vs) of
    Just arrType ->
      when (arrType /= expected) (Left (PropertyError property $ ArrayTypeMismatch expected arrType))
    Nothing ->
      let received = valueType v
      in Left (PropertyError property (ArrayTypeMismatch expected received))
checkArrayType expected property value =
  let received = valueType value
    in Left (PropertyError property (ArrayTypeMismatch expected received))

checkBoolean :: Property -> Value -> Either SemanticError Unit
checkBoolean _ (VBoolean _) = Right unit
checkBoolean property v =
  let received = valueType v
    in Left (PropertyError property (TypeMismatch TBoolean received))

checkNumber :: Property -> Value -> Either SemanticError Unit
checkNumber _ (VFloat _) = Right unit
checkNumber _ (VInteger _) = Right unit
checkNumber property v =
  let expected = L.fromFoldable [TFloat, TInteger]
      received = valueType v
    in Left (PropertyError property (TypesMismatch expected received))

checkInteger :: Property -> Value -> Either SemanticError Unit
checkInteger _ (VInteger _) = Right unit
checkInteger property value =
  let received = valueType value
    in Left (PropertyError property (TypeMismatch TInteger received))

checkProgram :: Program -> SemanticM Unit
checkProgram (Program { collection }) = checkCollection collection

checkCollection :: Collection -> SemanticM Unit
checkCollection = go
  where
    go (Collection
      { collectionProperties: properties
      , collectionGetters
      , collectionTable
      , collectionRequired
      }) = do
      let table = fromMaybe L.Nil collectionTable
      let getters = fromMaybe L.Nil collectionGetters
      let required = fromMaybe L.Nil collectionRequired

      checkProperties properties
      checkGetters getters

      local (extendContext properties getters) $ do
        checkTable table
        checkRequired required

    extendContext :: Properties -> Getters -> Context -> Context
    extendContext properties getters ctx =
      let ctx' = extendProperties ctx properties
        in extendGetters ctx' getters

    extendProperties :: Context -> Properties -> Context
    extendProperties = L.foldl
      \(Context {properties, getters}) property@(Property {propertyName: (PropertyName name)}) ->
        let properties' = extend name property properties
          in (Context { properties: properties', getters })

    extendGetters :: Context -> Getters -> Context
    extendGetters = L.foldl
      \(Context {properties, getters}) getter@(Getter {getterName: (PropertyName name)}) ->
        let getters' = extend name getter getters
          in (Context { getters: getters', properties })

checkRequired :: Required -> SemanticM Unit
checkRequired = traverse_ $ \(RequiredProperty propertyName condition) -> do
  property <- lookupProperty propertyName
  when (property == Nothing) (throwError (UndefinedProperty propertyName))
  case condition of
    Just expr -> checkExpr expr
    Nothing -> pure unit

checkTable :: Table -> SemanticM Unit
checkTable  = traverse_ $ \propertyName -> do
  property <- lookupProperty propertyName
  case property of
    Nothing -> do
      getter <- lookupGetter propertyName
      case getter of
        Nothing -> throwError (UndefinedProperty propertyName)
        Just _ -> pure unit
    Just _ -> pure unit

checkGetters :: Getters -> SemanticM Unit
checkGetters = traverse_ go
  where
    go getter@(Getter { getterMacro: (Macro (Name lang) _) })
      | lang `elem` ["js", "ts"] = pure unit
      | otherwise = throwError (InvalidMacroGetter getter)

checkProperties :: Properties -> SemanticM Unit
checkProperties = traverse_ \property -> do
  case checkProperty property of
    Right _ -> pure unit
    Left err -> throwError err

checkProperty :: Property -> Either SemanticError Unit
checkProperty property@(Property { propertyType }) =
  let validate = mkPropertyValidate propertyType
    in validate property

mkPropertyValidate :: PropertyType -> Property -> Either SemanticError Unit
mkPropertyValidate =
  case _ of
    PEnum -> checkEnumProperty
    PString -> checkStringProperty
    PFloat -> checkNumberProperty
    PInteger -> checkNumberProperty
    PFile -> checkFileProperty
    PBoolean -> checkBooleanProperty
    PCollection _ -> checkCollectionProperty
    PObject _ -> checkObjectProperty
    PArray _ -> checkArrayProperty

checkBooleanProperty :: Property -> Either SemanticError Unit
checkBooleanProperty property@(Property { propertyAttributes })
  | L.length propertyAttributes > 0 = Left (PropertyError property UnexpectedAttributes)
  | otherwise = pure unit

checkArrayProperty :: Property -> Either SemanticError Unit
checkArrayProperty property@(Property { propertyType, propertyAttributes }) =
  case propertyType of
    PArray propertyType' ->
        case propertyType' of
          PObject propeprties -> validate propeprties
          PCollection _ -> pure unit
          _ ->
            if L.length propertyAttributes > 0 then
              Left (PropertyError property UnexpectedAttributes)
            else pure unit
    _ -> Left Unknown
  where
    validate L.Nil = pure unit
    validate (p:ps) =
      case checkProperty p of
        Right _ -> validate ps
        Left err -> Left err

checkObjectProperty :: Property -> Either SemanticError Unit
checkObjectProperty property@(Property { propertyType, propertyAttributes })
  | L.length propertyAttributes > 0 = Left (PropertyError property UnexpectedAttributes)
  | otherwise =
    case propertyType of
      PObject properties -> validate properties
      _ -> Left Unknown
  where
    validate L.Nil = pure unit
    validate (p:ps) =
      case checkProperty p of
        Right _ -> validate ps
        Left err -> Left err

checkStringProperty :: Property -> Either SemanticError Unit
checkStringProperty property = checkAttributes property validations
  where
    validations :: M.Map String (Property -> Value -> Either SemanticError Unit)
    validations = M.fromFoldable
      [ "min" /\ checkInteger
      , "max" /\ checkInteger
      , "format" /\ checkFormat
      , "type" /\ checkType
      , "mask" /\ checkMask
      ]

    checkFormat :: Property -> Value -> Either SemanticError Unit
    checkFormat property' (VString format)
      | elem format formatOptions = Right unit
      | otherwise =
        let expected = L.fromFoldable formatOptions
          in Left (PropertyError property' (AttributeValueMustBe expected))
    checkFormat property' value =
      let received = valueType value
        in Left (PropertyError property' (TypeMismatch TString received))

    checkType :: Property -> Value -> Either SemanticError Unit
    checkType property' (VString typ)
      | elem typ typeOptions = Right unit
      | otherwise =
        let expected = L.fromFoldable typeOptions
          in Left (PropertyError property' (AttributeValueMustBe expected))
    checkType property' value =
      let received = valueType value
        in Left (PropertyError property' (TypeMismatch TString received))

    checkMask :: Property -> Value -> Either SemanticError Unit
    checkMask _ (VString _) = Right unit
    checkMask property' arr@(VArray _) = checkArrayType TString property' arr
    checkMask property' value =
      let received = valueType value
          expected = L.fromFoldable [TString, TArray]
        in Left (PropertyError property' (TypesMismatch expected received))

    typeOptions :: Array String
    typeOptions = ["text", "email", "password", "search", "time", "month"]

    formatOptions :: Array String
    formatOptions = ["date", "date-time"]

checkNumberProperty :: Property -> Either SemanticError Unit
checkNumberProperty property = checkAttributes property validations
  where
    validations = M.fromFoldable
      [ "min" /\ checkNumber
      , "max" /\ checkNumber
      , "exclusiveMinimum" /\ checkNumber
      , "exclusiveMaximum" /\ checkNumber
      ]

checkCollectionProperty :: Property -> Either SemanticError Unit
checkCollectionProperty property = checkAttributes property validations
  where
    validations = M.fromFoldable
      [ "indexes" /\ checkArrayType TProperty
      , "populate" /\ checkArrayType TProperty
      , "inline" /\ checkBoolean
      ]

checkFileProperty :: Property -> Either SemanticError Unit
checkFileProperty property = checkAttributes property validations
  where
    validations = M.fromFoldable
      [ "accept" /\ checkArrayType TString
      ]

checkEnumProperty :: Property -> Either SemanticError Unit
checkEnumProperty property = checkAttributes property validations
  where
    validations = M.fromFoldable
      [ "options" /\ checkArrayType TString
      ]

checkAttributes :: Property -> M.Map String (Property -> Value -> Either SemanticError Unit) -> Either SemanticError Unit
checkAttributes property@(Property { propertyAttributes }) validations =
  traverse_ check propertyAttributes
  where
    check :: Attribute -> Either SemanticError Unit
    check (Attribute attributeName@(Name name) value) =
      case M.lookup name validations of
        Just validation -> validation property value
        Nothing -> Left (PropertyError property (InvalidAttribute attributeName))

runSemantic :: Program -> Either SemanticError Unit
runSemantic program =
  let semantic = runReaderT (checkProgram program) emptyContext
    in runExcept semantic
