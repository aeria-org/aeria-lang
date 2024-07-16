import {
  Collection,
  SchemaWithId,
  ExtendCollection,
  get,
  insert,
  remove
} from "aeria";

export declare type collectionFunctionsTestCollection = {
  description: { $id: "collectionFunctionsTest"; properties: {} };
  functions: { get: typeof get; insert: typeof insert; remove: typeof remove };
  exposedFunctions: {
    customFunc2: true;
    get: "unauthenticated";
    insert: ["root"];
  };
};
export declare const collectionFunctionsTest: collectionFunctionsTestCollection & {
  item: SchemaWithId<collectionFunctionsTestCollection["description"]>;
};
export declare type CollectionFunctionsTest = SchemaWithId<
  typeof collectionFunctionsTest.description
>;
export declare const extendCollectionFunctionsTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionFunctionsTest, TCollection>;
