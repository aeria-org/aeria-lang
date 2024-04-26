module Aeria.Semantic.Error where

import Prelude

import Aeria.Syntax.Tree (AttributeName(..), CollectionName(..), Expr, FiltersPresetsItem, Ident, LayoutItem, Property(..), PropertyName(..), Typ)
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.String.Utils (ucfirst)

data PropertyError
  = PropertyTypeDoesNotExpectAttributes
  | PropertyTypeDoesNotExpectType
  | UndefinedReference CollectionName
  | TypeMismatch (Array Typ) Typ
  | ArrayTypeMismatch Typ Typ
  | UndefinedAttribute AttributeName
  | AttributeLiteralMustBe (L.List Ident)

derive instance genericPropertyError :: Generic PropertyError _

instance showPropertyError :: Show PropertyError where
  show PropertyTypeDoesNotExpectAttributes = "Property type does not expect attributes"
  show PropertyTypeDoesNotExpectType = "Property type does not expect a type"
  show (UndefinedReference name) = "Undefined reference to collection: " <> ppCollectionName name
  show (TypeMismatch expected got) = "Type mismatch, expected: " <> show expected <> ", got: " <> show got
  show (ArrayTypeMismatch expected got) = "Array type mismatch, expected: " <> show expected <> ", got: " <> show got
  show (UndefinedAttribute attrName) = "Undefined attribute: " <> ppAttributeName attrName
  show (AttributeLiteralMustBe idents) = "Attribute literal must be one of: " <> show idents

data ExprError
  = ExpectedProperty
  | NotProperty PropertyName

derive instance genericExprError :: Generic ExprError _

instance showExprError :: Show ExprError where
  show ExpectedProperty = "Expected a property"
  show (NotProperty (PropertyName _ name)) = "Not a property: \"" <> name <> "\""

data SemanticError
  = ExprError Expr ExprError
  | UndefinedProperty PropertyName
  | PropertyIsAlreadyInUse PropertyName
  | PropertyError Property PropertyError
  | FiltersPresetsError FiltersPresetsItem
  | LayoutComponentError LayoutItem
  | UndefinedFunction PropertyName
  | UndefinedStrategy String

derive instance genericSemanticError :: Generic SemanticError _

instance showSemanticError :: Show SemanticError where
  show (ExprError _ exprErr) = show exprErr
  show (UndefinedProperty (PropertyName _ name)) = "Undefined property: \"" <> name <> "\""
  show (PropertyError (Property { name }) propErr) = "Error in property \"" <> ppPropertyName name <> "\": " <> show propErr
  show (FiltersPresetsError _) = "\"Filters\" is required"
  show (LayoutComponentError _) = "\"Component name\" is required"
  show (UndefinedFunction (PropertyName _ name)) = "Undefined function: \"" <> name <> "\""
  show (UndefinedStrategy strategy) = "Undefined strategy: \"" <> strategy <> "\""
  show (PropertyIsAlreadyInUse _) = "PropertyIsAlreadyInUse"

ppPropertyName :: PropertyName -> String
ppPropertyName (PropertyName _  propertyName) = propertyName

ppCollectionName :: CollectionName -> String
ppCollectionName (CollectionName _  collectionName) = ucfirst collectionName

ppAttributeName :: AttributeName -> String
ppAttributeName (AttributeName _  attributeName) = attributeName
