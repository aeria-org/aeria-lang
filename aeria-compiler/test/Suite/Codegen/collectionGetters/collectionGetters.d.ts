import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

export declare type collectionGettersCollection = {
  description: {
    $id: "collectionGetters";
    properties: {
      prop1: { type: "integer" };
      prop2: { type: "integer" };
      sum: { getter: (doc: any) => any };
    };
  };
};
export declare const collectionGetters: collectionGettersCollection & {
  item: SchemaWithId<collectionGettersCollection["description"]>;
};
export declare type CollectionGetters = SchemaWithId<
  typeof collectionGetters.description
>;
export declare const extendCollectionGettersCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionGetters["description"]>
      ) => unknown;
    };
  }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionGetters, TCollection>;
