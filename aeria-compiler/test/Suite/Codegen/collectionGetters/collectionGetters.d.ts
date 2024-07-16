import { Collection, SchemaWithId, ExtendCollection } from "aeria";

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
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionGetters, TCollection>;
