import { Collection, SchemaWithId, ExtendCollection } from "aeria";

export declare type collectionPropertyEnumTestCollection = {
  description: {
    $id: "collectionPropertyEnumTest";
    properties: { prop1: { enum: ["value1", "value2", "value3"] } };
  };
};
export declare const collectionPropertyEnumTest: collectionPropertyEnumTestCollection & {
  item: SchemaWithId<collectionPropertyEnumTestCollection["description"]>;
};
export declare type CollectionPropertyEnumTest = SchemaWithId<
  typeof collectionPropertyEnumTest.description
>;
export declare const extendCollectionPropertyEnumTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyEnumTest, TCollection>;
