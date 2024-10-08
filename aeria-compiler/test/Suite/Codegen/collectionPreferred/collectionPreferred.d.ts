import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

export declare type collectionPreferredCollection = {
  description: {
    $id: "collectionPreferred";
    properties: { prop1: { type: "string" }; prop2: { type: "string" } };
    preferred: { role1: { table: ["prop1"] }; role2: { table: ["prop2"] } };
  };
};
export declare const collectionPreferred: collectionPreferredCollection & {
  item: SchemaWithId<collectionPreferredCollection["description"]>;
};
export declare type CollectionPreferred = SchemaWithId<
  typeof collectionPreferred.description
>;
export declare const extendCollectionPreferredCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPreferred["description"]>
      ) => unknown;
    };
  }
>(
  collection: Pick<TCollection, keyof Collection>
) => ExtendCollection<typeof collectionPreferred, TCollection>;
