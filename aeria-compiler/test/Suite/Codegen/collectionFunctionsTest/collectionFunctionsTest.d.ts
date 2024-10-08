import {
  Collection,
  SchemaWithId,
  ExtendCollection,
  Context,
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
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionFunctionsTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: Pick<TCollection, keyof Collection>
) => ExtendCollection<typeof collectionFunctionsTest, TCollection>;
