import { Collection, SchemaWithId, ExtendCollection } from "aeria";
export declare type collectionpropertystrtestCollection = {
  description: {
    $id: "collectionpropertystrtest";
    properties: { prop1: { type: "string" } };
  };
};
export declare const collectionpropertystrtest: collectionpropertystrtestCollection & {
  item: SchemaWithId<collectionpropertystrtestCollection["description"]>;
};
export declare type Collectionpropertystrtest = SchemaWithId<
  typeof collectionpropertystrtest.description
>;
export declare const extendCollectionpropertystrtestCollection: <
  const TCollection extends {
    [P in keyof Collection]?: Partial<Collection[P]>;
  },
>(
  collection: TCollection,
) => ExtendCollection<typeof collectionpropertystrtest, TCollection>;
