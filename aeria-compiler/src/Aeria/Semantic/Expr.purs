module Aeria.Semantic.Expr where

import Prelude

import Aeria.Semantic.Internal (Context, collectionHasProperty)
import Aeria.Syntax.Tree (CollectionName, Expr(..), Literal(..), PropertyName, getName)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

sExpr :: Context -> CollectionName -> Expr -> Either String Unit
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
    ENot expr'    -> sUnaryExpr expr'
    ETruthy expr' -> sUnaryExpr expr'
    ELiteral _    -> pure unit
  where
    sUnaryExpr :: Expr -> Either String Unit
    sUnaryExpr (ELiteral (LProperty _ propertyName)) = collectionHasProperty' propertyName
    sUnaryExpr _ = Left "Expected a property name in the expression"

    sBinaryExpr :: Expr -> Expr -> Either String Unit
    sBinaryExpr (ELiteral (LProperty _ propertyName)) rgt = do
      collectionHasProperty' propertyName
      sExpr context collectionName rgt
    sBinaryExpr (ELiteral _) _ = Left "Expected a property name to the left of the expression"
    sBinaryExpr lft rgt = do
      sExpr context collectionName lft
      sExpr context collectionName rgt

    collectionHasProperty' :: PropertyName -> Either String Unit
    collectionHasProperty' propertyName =
      case collectionHasProperty context collectionName propertyName of
        Just _ -> pure unit
        Nothing -> Left ("Property \"" <> getName propertyName <> "\" does not exist on collection \"" <> getName collectionName <> "\"")
