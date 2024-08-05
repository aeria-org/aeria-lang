module Aeria.Codegen where

import Aeria.Syntax.Tree

import Aeria.Codegen.Collection (cCollection)
import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Type (typegen)
import Aeria.Codegen.Typescript.Tree as Ts
import Data.List as L
import Data.Maybe (Maybe(..), isJust)
import Data.String.Utils (ucLower)
import Data.Tuple.Nested ((/\))
import Prelude (map, not, (#), ($), (<>))

data Codegen = Codegen String Js.Statements Ts.Statements

codegen :: Program -> L.List Codegen
codegen (Program { collections }) = map go collections
  where
    go collection@(Collection { name: collectionName, functions, extends }) =
      Codegen (getName collectionName) js ts
      where
        functions' = getAeriaFunctions functions

        tsImportsFunctions =
          map Ts.importSpecifier1 functions'
            # L.toUnfoldable
        jsImportsFunctions =
          map Js.importSpecifier1 functions'
            # L.toUnfoldable

        collectionObject = cCollection collection
        collectionObjectType = typegen collectionObject

        ts = mkTsFile extends collectionName collectionObjectType tsImportsFunctions
        js = mkJsFile extends collectionName collectionObject jsImportsFunctions

getAeriaFunctions :: L.List FunctionItem -> L.List String
getAeriaFunctions functions =
  functions
    # L.filter (\(FunctionItem { custom }) -> not custom)
    # map (\(FunctionItem { functionName }) -> getName functionName)

mkTsFile :: Maybe ExtendsName -> CollectionName -> Ts.Tree -> Array Ts.ImportSpecifier -> Ts.Statements
mkTsFile extends collectionName collectionType functions = Ts.statements statements
  where
    base =
      [ mkTsImports functions
      , mkTsCollectionType extends collectionName collectionType
      , mkTsDescription collectionName
      , mkTsSchemaType collectionName
      , mkTsExtendCollectionFunction collectionName
      ]

    statements =
      case mkTsExtendsImport extends of
        Just extendsImport -> [extendsImport] <> base
        Nothing -> base
mkTsImports :: Array Ts.ImportSpecifier -> Ts.Statement
mkTsImports functions =
  Ts.importDeclaration "aeria" (Ts.specifiers $ base <> functions)
  where
    base =
      [ Ts.importSpecifier1 "Collection"
      , Ts.importSpecifier1 "SchemaWithId"
      , Ts.importSpecifier1 "ExtendCollection"
      , Ts.importSpecifier1 "Context"
      ]

mkTsExtendsImport :: Maybe ExtendsName -> Maybe Ts.Statement
mkTsExtendsImport (Just (ExtendsName package collection'')) =
  Just $ Ts.importDeclaration package
    (Ts.specifiers [Ts.importSpecifier2 (ucLower collection'') "original"])
mkTsExtendsImport Nothing = Nothing

mkTsCollectionType :: Maybe ExtendsName -> CollectionName -> Ts.Tree -> Ts.Statement
mkTsCollectionType extends collectionName collectionType =
  Ts.exportDeclareDeclaration
    (Ts.typeAliasDeclaration
      (getName collectionName <> "Collection")
      (if isJust extends
        then Ts.type_
          ( Ts.typeGeneric
            [ Ts.typeQuery (Ts.variable "original")
            , collectionType
            ]
            (Ts.type_ $ Ts.typeVariable "ExtendCollection"))
        else collectionType))

mkTsDescription :: CollectionName -> Ts.Statement
mkTsDescription collectionName =
  Ts.exportDeclareDeclaration
    (Ts.variableDeclaration
      (getName collectionName)
      (Ts.intersection
        (Ts.type_ $ Ts.typeVariable ((getName collectionName) <> "Collection"))
        (Ts.type_ (Ts.typeObject
          [Ts.typeObjectProperty
            (Ts.identifier "item")
            (Ts.type_
              (Ts.typeGeneric
                [ Ts.type_ $ Ts.typeRaw ((getName collectionName) <> "Collection[\"description\"]")]
                (Ts.type_ $ Ts.typeVariable "SchemaWithId")))]))))

mkTsSchemaType :: CollectionName -> Ts.Statement
mkTsSchemaType collectionName@(CollectionName _ collectionName') =
  Ts.exportDeclareDeclaration
    (Ts.typeAliasDeclaration
      collectionName'
      (Ts.type_ (Ts.typeGeneric
        [Ts.typeQuery (Ts.variable (getName collectionName <> ".description"))]
        (Ts.type_ $ Ts.typeVariable  "SchemaWithId")
      )))

mkTsExtendCollectionFunction :: CollectionName -> Ts.Statement
mkTsExtendCollectionFunction collectionName@(CollectionName _ collectionName') =
  Ts.exportDeclareDeclaration
    (Ts.variableDeclaration
      ("extend" <> collectionName' <> "Collection")
      (Ts.type_ $ Ts.typeGeneric
        [ Ts.type_ $ Ts.typeRaw $ """
          const TCollection extends {
            [P in Exclude<keyof Collection, "functions">] ? : Partial <Collection[P]>
          } &{
            functions?: {
              [F: string]: (payload: any, context: Context<typeof """ <> getName collectionName <> """["description"]>) => unknown
            }
          }"""
        ]
        (Ts.type_ $ Ts.typeFunction
          [ (Ts.identifier "collection") /\ (Ts.type_ $ Ts.typeVariable "TCollection") ]
          (Ts.type_ $ (Ts.typeGeneric
            [ Ts.typeQuery (Ts.type_ $ Ts.typeVariable (getName collectionName))
            , Ts.type_ $ Ts.typeVariable "TCollection"
            ]
            (Ts.type_ $ Ts.typeVariable "ExtendCollection"))))))

mkJsFile :: Maybe ExtendsName -> CollectionName -> Js.Tree -> Array Js.ImportSpecifier -> Js.Statements
mkJsFile extends collectionName collectionObject functions = Js.statements statements
  where
    base =
      [ mkJsImports functions
      , mkJsDefineCollection extends collectionName collectionObject
      , mkJsExtendCollectionFunction collectionName
      ]
    statements =
      case mkJsExtendsImport extends of
        Just extendsImport -> [extendsImport] <> base
        Nothing -> base

mkJsImports :: Array Js.ImportSpecifier -> Js.Statement
mkJsImports imports =
  Js.importDeclaration "aeria" (Js.specifiers $ base <> imports)
  where
    base =
      [ Js.importSpecifier1 "extendCollection"
      , Js.importSpecifier1 "defineCollection"
      ]

mkJsExtendsImport :: Maybe ExtendsName -> Maybe Js.Statement
mkJsExtendsImport (Just (ExtendsName package collection'')) =
  Just $
    Js.importDeclaration package
      (Js.specifiers [ Js.importSpecifier2 (ucLower collection'') "original"])
mkJsExtendsImport Nothing = Nothing

mkJsDefineCollection :: Maybe ExtendsName -> CollectionName -> Js.Tree -> Js.Statement
mkJsDefineCollection extends collectionName collection' =
  Js.exportDeclaration
    (Js.variableDeclaration
      (getName collectionName)
      (if isJust extends
        then Js.call (Js.variable "extendCollection") [Js.variable "original", collection']
        else Js.call (Js.variable "defineCollection") [collection']))

mkJsExtendCollectionFunction :: CollectionName -> Js.Statement
mkJsExtendCollectionFunction collectionName@(CollectionName _ collectionName') =
  Js.exportDeclaration
    (Js.variableDeclaration
      ("extend" <> collectionName' <> "Collection")
      (Js.function
        [Js.identifier "collection"]
        (Js.call (Js.variable "extendCollection")
          [Js.variable (getName collectionName), Js.variable "collection"]
        )))
