module Aeria.Codegen.Javascript
  ( Codegen(..)
  , codegen
  )
  where

import Prelude

import Aeria.Codegen.Javascript.Tree (JsIdentifier(..), JsImportSpecifier(..), JsLiteral(..), JsObjectProperty(..), JsSpecifiers(..), JsStatement(..), JsStatements(..), JsTree(..))
import Aeria.Codegen.Type (codegenType)
import Aeria.Codegen.Typescript.Tree (TsIdentifier(..), TsImportSpecifier(..), TsParameter(..), TsSpecifiers(..), TsStatement(..), TsStatementSyntax(..), TsStatements(..), TsType(..), TsTypeParameter(..))
import Aeria.Syntax.Tree (Attribute(..), AttributeName(..), AttributeValue(..), Collection(..), CollectionName(..), Expr(..), Getter(..), Getters, Literal(..), Macro(..), Program(..), Properties, Property(..), PropertyName(..), PropertyType(..), Required, RequiredProperty(..), Table)
import Control.Lazy (fix)
import Data.Array (union)
import Data.CodePoint.Unicode (toUpper)
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..), isNothing)
import Data.String (fromCodePointArray, uncons)

data Codegen
  = Codegen String JsStatements TsStatements

ucfirst :: String -> String
ucfirst str = case str of
  "" -> ""
  _ ->
    case uncons str of
      Just { head, tail } -> fromCodePointArray (toUpper head) <> tail
      Nothing -> ""

codegen :: Program -> L.List Codegen
codegen (Program { collections }) = map go collections
  where
  go collection@(Collection { name: (CollectionName collectionName) }) = Codegen collectionName jsFile tsFile
    where
    collection' = codegenCollection collection

    tsFile =
      TsStatements
        $ L.fromFoldable
            [ TSImportDeclaration
                (TsSpecifiers (L.fromFoldable [ TsImportSpecifier (TsIdentifier "Collection"), TsImportSpecifier (TsIdentifier "SchemaWithId") ]))
                (TsIdentifier "@sonata-api/types")
            , TSExportNamedDeclaration
                ( TSVariableStatement (L.fromFoldable [ TsDeclareKeyword, TsConstKeyword ])
                    (TsIdentifier collectionName)
                    ((codegenType collection'))
                )
            , TSExportNamedDeclaration
                ( TSTypeAliasDeclaration
                    (TsIdentifier (ucfirst collectionName))
                    ( TSTypeReference
                        ( L.fromFoldable
                            [ TsTypeParameter $ TSTypeQuery (TsIdentifier (collectionName <> ".description")) ]
                        )
                        (TsIdentifier "SchemaWithId")
                    )
                )
            , TSExportNamedDeclaration
                ( TSTypeAliasDeclaration
                    (TsIdentifier "extendCollection")
                    ( TSFunctionType
                        ( L.fromFoldable
                            [ TsTypeParameter
                                $ TSTypeExtends
                                    (TSTypeReference L.Nil (TsIdentifier "T"))
                                    (TSTypeReference L.Nil (TsIdentifier "Collection"))
                            ]
                        )
                        (L.fromFoldable [ TsParameter (TsIdentifier "collection") (TSTypeReference L.Nil (TsIdentifier "T")) ])
                        ( TSIntersectionType
                            (TSTypeReference L.Nil (TsIdentifier "T"))
                            (TSTypeQuery (TsIdentifier collectionName))
                        )
                    )
                )
            ]

    jsFile =
      ( JsStatements
          $ L.fromFoldable
              [ JSImportDeclaration
                  ( JsSpecifiers
                      $ L.fromFoldable
                          [ JsImportSpecifier (JsIdentifier "defineCollection")
                          , JsImportSpecifier (JsIdentifier "deepMerge")
                          ]
                  )
                  (JsIdentifier "sonata-api")
              , JSExportNamedDeclaration (JSVariableDeclaration (JsIdentifier collectionName) collection')
              , JSExportNamedDeclaration
                  ( JSVariableDeclaration
                      (JsIdentifier "extendCollection")
                      ( JSArrowFunctionExpression
                          (L.fromFoldable [ JsIdentifier "collection" ])
                          ( JSCallExpression (JsIdentifier "defineCollection")
                              ( L.fromFoldable
                                  [ ( JSCallExpression (JsIdentifier "deepMerge")
                                        (L.fromFoldable [ JSIdentifier (JsIdentifier collectionName), JSIdentifier (JsIdentifier "collection") ])
                                    )
                                  ]
                              )
                          )
                      )
                  )
              ]
      )

codegenCollection :: Collection -> JsTree
codegenCollection ( Collection
  { name: (CollectionName collectionName)
  , required
  , properties
  , getters
  , table
  }
) =
  JSLiteral $ JSObject
    $ L.fromFoldable
        [ makeObjectProperty "description" codegenDescription
        ]
  where
  codegenDescription = JSLiteral (JSObject (L.fromFoldable description))
    where
    description = union (union baseDescription tableDescription) requiredDescription

    baseDescription =
      [ makeObjectProperty "$id" (JSLiteral (JSString collectionName))
      , makeObjectProperty "properties" (codegenProperties properties getters)
      ]

    tableDescription = case table of
      L.Nil -> []
      _ -> [ makeObjectProperty "table" (codegenTable table) ]

    requiredDescription = case required of
      L.Nil -> []
      _ -> [ makeObjectProperty "required" (codegenRequired required) ]

codegenTable :: Table -> JsTree
codegenTable table = JSLiteral (JSArray (map (\(PropertyName propertyName) -> JSLiteral $ JSString propertyName) table))

codegenGetters :: Getters -> L.List JsObjectProperty
codegenGetters getters = map go getters
  where
  go (Getter { name: (PropertyName name), macro: (Macro _ code) }) =
    makeObjectProperty name
      ( JSLiteral $ JSObject
          $ L.fromFoldable
              [ makeObjectProperty "getter" (JSArrowFunctionExpression (L.fromFoldable [JsIdentifier "doc"]) (JSCode code))
              ]
      )

codegenRequired :: Required -> JsTree
codegenRequired required =
  if hasCondition then
    JSLiteral $ JSArray $ map (\(RequiredProperty (PropertyName propertyName) _) -> JSLiteral (JSString propertyName)) required
  else
    JSLiteral $ JSObject
      $ map
          ( \(RequiredProperty (PropertyName propertyName) expr) ->
              makeObjectProperty propertyName (codegenObject expr)
          )
          required
  where
  hasCondition = L.all (\(RequiredProperty _ cond) -> isNothing cond) required

  codegenObject Nothing = JSLiteral (JSBoolean true)

  codegenObject (Just cond) = codegenExpr cond

codegenBinaryExpr :: String -> JsTree -> JsTree -> JsTree
codegenBinaryExpr oper e1 e2 =
  JSLiteral $ JSObject
    $ L.fromFoldable
        [ makeObjectProperty "operator" (JSLiteral $ JSString oper)
        , makeObjectProperty "term1" e1
        , makeObjectProperty "term2" e2
        ]

codegenUnaryExpr :: String -> JsTree -> JsTree
codegenUnaryExpr oper e1 =
  JSLiteral $ JSObject
    $ L.fromFoldable
        [ makeObjectProperty "operator" (JSLiteral (JSString oper))
        , makeObjectProperty "term1" e1
        ]

codegenExpr :: Expr -> JsTree
codegenExpr (ELiteral value) = codegenLiteral value
codegenExpr (EExists e1) = codegenUnaryExpr "exists" (codegenExpr e1)
codegenExpr (ENot e1) = codegenUnaryExpr "not" (codegenExpr e1)
codegenExpr (EOr e1 e2) =
  JSLiteral $ JSObject
    $ L.fromFoldable
        [ makeObjectProperty "or" (JSLiteral (JSArray (codegenExpr e1 L.: codegenExpr e2 L.: L.Nil)))
        ]
codegenExpr (EAnd e1 e2) =
  JSLiteral $ JSObject
    $ L.fromFoldable
        [ makeObjectProperty "and" (JSLiteral (JSArray (codegenExpr e1 L.: codegenExpr e2 L.: L.Nil)))
        ]
codegenExpr (EIn e1 e2) = codegenBinaryExpr "in" (codegenExpr e1) (codegenExpr e2)
codegenExpr (ELt e1 e2) = codegenBinaryExpr "lt" (codegenExpr e1) (codegenExpr e2)
codegenExpr (EGt e1 e2) = codegenBinaryExpr "gt" (codegenExpr e1) (codegenExpr e2)
codegenExpr (ELte e1 e2) = codegenBinaryExpr "lte" (codegenExpr e1) (codegenExpr e2)
codegenExpr (EGte e1 e2) = codegenBinaryExpr "gte" (codegenExpr e1) (codegenExpr e2)
codegenExpr (EEq e1 e2) = codegenBinaryExpr "eq" (codegenExpr e1) (codegenExpr e2)

codegenProperties :: Properties -> Getters -> JsTree
codegenProperties properties getters = JSLiteral (JSObject (properties' `L.union` getters'))
  where
  codegenProperty :: Property -> JsObjectProperty
  codegenProperty property@( Property
    { name: (PropertyName propertyName)
    }
  ) = makeObjectProperty propertyName (JSLiteral (JSObject $ go property))

  properties' = map codegenProperty properties

  getters' = codegenGetters getters

  codegenAttributes =
    map
      ( \(Attribute (AttributeName attributeName) value) ->
        case value of
          ALiteral value' -> makeObjectProperty attributeName (codegenLiteral value')
          AExpr expr -> makeObjectProperty attributeName (codegenExpr expr)
      )

  go :: Property -> L.List JsObjectProperty
  go (Property { name, type_, attributes }) =
    let
      attributes' = codegenAttributes attributes
    in
      case type_ of
        PEnum ->
          let
            options = L.find (\(Attribute (AttributeName x) _) -> x == "options") attributes
          in
            case options of
              Just (Attribute (AttributeName _) (ALiteral (LArray value))) ->
                L.fromFoldable [makeObjectProperty "enum" (JSLiteral (JSArray (map codegenLiteral value))) ]
              _ -> L.fromFoldable [makeObjectProperty "enum" (JSLiteral (JSArray L.Nil)) ]
        PFloat -> L.fromFoldable [makeObjectProperty "type" (JSLiteral (JSString "number")) ] `L.union` attributes'
        PInteger -> L.fromFoldable [makeObjectProperty "type" (JSLiteral (JSString "integer")) ] `L.union` attributes'
        PString -> L.fromFoldable [makeObjectProperty "type" (JSLiteral (JSString "string")) ] `L.union` attributes'
        PBoolean -> L.fromFoldable [makeObjectProperty "type" (JSLiteral (JSString "boolean")) ] `L.union` attributes'
        PArray type_' ->
          L.fromFoldable
            [ makeObjectProperty "type" (JSLiteral (JSString "array"))
            , makeObjectProperty "items" (JSLiteral (JSObject (go (Property { name, type_: type_', attributes }) `L.union` attributes')))
            ]
        PObject properties'' ->
          L.fromFoldable
            [ makeObjectProperty "type" (JSLiteral (JSString "object"))
            , makeObjectProperty "properties" (codegenProperties properties'' L.Nil)
            ] `L.union` attributes'
        PRef (CollectionName collection) -> L.fromFoldable
          [ makeObjectProperty "$ref" (JSLiteral (JSString collection))
          ] `L.union` attributes'

codegenLiteral :: Literal -> JsTree
codegenLiteral =
  fix \self -> case _ of
    (LInteger integer) -> JSLiteral (JSNumber (toNumber integer))
    (LFloat float) -> JSLiteral (JSNumber float)
    (LString str) -> JSLiteral (JSString str)
    (LBoolean bool) -> JSLiteral (JSBoolean bool)
    (LArray arr) -> JSLiteral (JSArray (map self arr))
    (LProperty (PropertyName propertyName)) -> JSLiteral (JSString propertyName)

makeObjectProperty :: String -> JsTree -> JsObjectProperty
makeObjectProperty ident tree = JsObjectProperty (JsIdentifier ident) tree
