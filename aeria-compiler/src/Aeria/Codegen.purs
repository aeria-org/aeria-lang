module Aeria.Codegen where

import Prelude (map, not, (#), ($), (<>))

import Aeria.Syntax.Tree
import Aeria.Codegen.Collection (cCollection)
import Aeria.Codegen.Type (typegen)
import Aeria.Codegen.Javascript.Tree as Js
import Aeria.Codegen.Typescript.Tree as Ts
import Data.List as L
import Data.Maybe (Maybe(..), isJust)
import Data.String.Utils (ucLower)
import Data.Tuple.Nested ((/\))

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
mkTsFile extends collectionName collectionType functions =
  Ts.statements
    [ mkTsImports functions
    , mkTsExtendsImport extends
    , mkTsCollectionType extends collectionName collectionType
    , mkTsDescription collectionName
    , mkTsSchemaType collectionName
    , mkTsExtendCollectionFunction collectionName
    ]

mkTsImports :: Array Ts.ImportSpecifier -> Ts.Statement
mkTsImports functions =
  Ts.importDeclaration "aeria" (Ts.specifiers $ base <> functions)
  where
    base =
      [ Ts.importSpecifier1 "Collection"
      , Ts.importSpecifier1 "SchemaWithId"
      , Ts.importSpecifier1 "ExtendCollection"
      ]

mkTsExtendsImport :: Maybe ExtendsName -> Ts.Statement
mkTsExtendsImport (Just (ExtendsName package collection'')) =
  Ts.importDeclaration package
    (Ts.specifiers [Ts.importSpecifier2 (ucLower collection'') "original"])
mkTsExtendsImport Nothing = Ts.emptyStatement

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
        [ (Ts.extends
            (Ts.type_ $ Ts.typeRaw "const TCollection")
            (Ts.type_ $ Ts.typeRaw "{ [P in keyof Collection]?: Partial<Collection[P]> }"))]
        (Ts.type_ $ Ts.typeFunction
          [ (Ts.identifier "collection") /\ (Ts.type_ $ Ts.typeVariable "TCollection") ]
          (Ts.type_ $ (Ts.typeGeneric
            [ Ts.typeQuery (Ts.type_ $ Ts.typeVariable (getName collectionName))
            , Ts.type_ $ Ts.typeVariable "TCollection"
            ]
            (Ts.type_ $ Ts.typeVariable "ExtendCollection"))))))

mkJsFile :: Maybe ExtendsName -> CollectionName -> Js.Tree -> Array Js.ImportSpecifier -> Js.Statements
mkJsFile extends collectionName collectionObject functions =
  Js.statements
    [ mkJsImports functions
    , mkJsExtendsImport extends
    , mkJsDefineCollection extends collectionName collectionObject
    , mkJsExtendCollectionFunction collectionName
    ]

mkJsImports :: Array Js.ImportSpecifier -> Js.Statement
mkJsImports imports =
  Js.importDeclaration "aeria" (Js.specifiers $ base <> imports)
  where
    base =
      [ Js.importSpecifier1 "extendCollection"
      , Js.importSpecifier1 "defineCollection"
      ]

mkJsExtendsImport :: Maybe ExtendsName -> Js.Statement
mkJsExtendsImport (Just (ExtendsName package collection'')) =
  Js.importDeclaration package
    (Js.specifiers [ Js.importSpecifier2 (ucLower collection'') "original"])
mkJsExtendsImport Nothing = Js.emptyStatement

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
