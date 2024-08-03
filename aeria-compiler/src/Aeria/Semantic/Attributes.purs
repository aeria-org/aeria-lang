module Aeria.Semantic.Attributes where

import Prelude

import Aeria.Semantic.Expr (sExpr)
import Aeria.Semantic.Internal (Context, SemanticM, collectionHasProperty, throwDiagnostic)
import Aeria.Syntax.Tree (Attribute(..), AttributeName, AttributeValue(..), CollectionName, Literal(..), Property(..), PropertyType(..), getName)
import Control.Monad.Reader (ask)
import Data.Array as A
import Data.Either (Either(..), either, isRight)
import Data.Foldable (traverse_)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.Utils (concatWith)

data AttributeType
  = AttrString
  | AttrUndefined
  | AttrNull
  | AttrInteger
  | AttrNum
  | AttrBoolean
  | AttrExpr
  | AttrPropertyName
  | AttrEnum (Array String)
  | AttrArrayOf AttributeType
  | AttrArray

instance showattrType :: Show AttributeType where
  show AttrString = "string"
  show AttrUndefined = "undefined"
  show AttrNull = "null"
  show AttrInteger = "integer"
  show AttrNum = "number"
  show AttrBoolean = "boolean"
  show AttrExpr = "expression"
  show AttrPropertyName = "property name"
  show (AttrEnum values) = "string in " <> show values
  show (AttrArrayOf type') = "array of " <> show type'
  show AttrArray = "array"

data AttributeConstraint
  = SingleType AttributeType
  | MultipleTypes (Array AttributeType)

instance showAttributeConstraint :: Show AttributeConstraint where
  show (SingleType attrType) = show attrType
  show (MultipleTypes attrTypes) = "(" <> concatWith attrTypes ", " show <> ")"

type AttributeConstraints = M.Map String AttributeConstraint

validatePropertyAttributes :: CollectionName -> Property -> AttributeConstraints -> SemanticM Unit
validatePropertyAttributes collectionName (Property {type_: propertyType, attributes }) attrConstraints = traverse_ validateAttribute attributes
  where
    validateAttribute :: Attribute -> SemanticM Unit
    validateAttribute attribute@(Attribute span attributeName _) = do
      context <- ask
      case M.lookup (getName attributeName) attrConstraints of
        Just validation -> applyAttributeConstraint context collectionName propertyType attribute validation
        Nothing -> throwDiagnostic span $ "Unknown attribute: \"" <> getName attributeName <> "\""

applyAttributeConstraint :: Context -> CollectionName -> PropertyType -> Attribute -> AttributeConstraint -> SemanticM Unit
applyAttributeConstraint context collectionName propertyType attribute@(Attribute span attributeName _) validation =
  case validation of
    SingleType attrType ->
      case checkAttributeType context collectionName propertyType attribute attrType of
        Left err -> throwDiagnostic span err
        Right _ -> pure unit
    MultipleTypes attrTypes ->
      if A.any (isRight <<< checkAttributeType context collectionName propertyType attribute) attrTypes
        then pure unit
        else throwDiagnostic span $ "Attribute \"" <> getName attributeName <> "\" must be " <> show validation <> ", but got " <> show (getAttributeValue attribute)

checkAttributeType :: Context -> CollectionName -> PropertyType -> Attribute -> AttributeType -> Either String Unit
checkAttributeType context collectionName propertyType attribute@(Attribute _ attributeName attributeValue) attrType =
  case attrType of
    AttrExpr -> validateExpr context collectionName attribute
    AttrEnum values -> validateEnum attribute values
    _ -> case attributeValue of
      ALiteral _ literal -> validateLiteralType context collectionName propertyType attributeName literal attrType
      _ -> Left $ "Attribute \"" <> getName attributeName <> "\" expected a literal, got " <> show attributeValue

validateExpr :: Context -> CollectionName -> Attribute -> Either String Unit
validateExpr context collectionName (Attribute _ attributeName value) =
  case value of
    AExpr _ expr -> case sExpr context collectionName expr of
      Left err -> Left $ "Attribute \"" <> getName attributeName <> "\" expression error: " <> err
      Right _ -> pure unit
    _ -> Left $ "Attribute \"" <> getName attributeName <> "\" must be an expression, but got " <> show value

validateEnum :: Attribute -> Array String -> Either String Unit
validateEnum (Attribute _ attributeName value) values =
  case value of
    ALiteral _ (LString _ str) ->
      if A.elem str values
        then pure unit
        else Left $ "Attribute \"" <> getName attributeName <> "\" must be one of: " <> show values <> ", but got \"" <> str <> "\""
    _ -> Left $ "Attribute \"" <> getName attributeName <> "\" must be a string, but got " <> show value

validateArrayElements :: Context -> CollectionName -> PropertyType -> AttributeName -> L.List Literal -> AttributeType -> Either String Unit
validateArrayElements context collectionName propertyType attributeName elements attrType = do
  -- traceM $ "validateArrayElements: " <> show elements
  let
    attrConstraints = A.mapWithIndex (\index literal ->
      validateLiteralType context collectionName propertyType attributeName literal attrType
      # either (Left <<< formatError index) Right
    ) (L.toUnfoldable elements)
    errors = A.catMaybes $ map (either Just (const Nothing)) attrConstraints
  case A.head errors of
    Just err -> Left err
    Nothing -> Right unit
  where
    formatError :: Int -> String -> String
    formatError index errMsg =
      "In attribute \"" <> getName attributeName <> "\", array element at index " <> show index <> ": " <> errMsg

validateLiteralType :: Context -> CollectionName -> PropertyType -> AttributeName -> Literal -> AttributeType -> Either String Unit
validateLiteralType context collectionName propertyType attributeName literal attrType = do
  -- traceM $ "validateLiteralType: " <> show literal <> " as " <> show attrType
  case attrType, literal of
    AttrString, LString _ _ -> pure unit
    AttrUndefined, LUndefined _ -> pure unit
    AttrNull, LNull _ -> pure unit
    AttrInteger, LInteger _ _ -> pure unit
    AttrNum, LNum _ _ -> pure unit
    AttrBoolean, LBoolean _ _ -> pure unit
    AttrPropertyName, LProperty _ propertyName ->
      case collectionHasProperty context collectionName propertyName of
        Just _ -> pure unit
        Nothing -> Left $ "Property \"" <> getName propertyName <> "\" not found in collection \"" <> getName collectionName <> "\""
    AttrArrayOf type', LArray _ values ->
      validateArrayElements context collectionName propertyType attributeName values type'
    AttrArray, LArray _ values -> do
      arrayConstraint <- getArrayConstraint propertyType
      validateArrayElements context collectionName propertyType attributeName values arrayConstraint
    _, _ -> Left $ "Expected " <> show attrType <> ", but got " <> show literal

getAttributeValue :: Attribute -> AttributeValue
getAttributeValue (Attribute _ _ value) = value

getArrayConstraint :: PropertyType -> Either String AttributeType
getArrayConstraint (PNum _) = Right AttrNum
getArrayConstraint (PInteger _) = Right AttrInteger
getArrayConstraint (PString _) = Right AttrString
getArrayConstraint (PBoolean _) = Right AttrBoolean
getArrayConstraint (PRef _ _) = Right AttrPropertyName
getArrayConstraint (PArray _ arrType) = getArrayConstraint arrType
getArrayConstraint _ = Left "Invalid array type"
