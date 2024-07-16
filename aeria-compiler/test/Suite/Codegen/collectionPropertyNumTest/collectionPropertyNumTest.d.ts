import { Collection, SchemaWithId, ExtendCollection } from "aeria";

export declare type collectionPropertyNumTestCollection = {
  description: {
    $id: "collectionPropertyNumTest";
    properties: {
      prop1: {
        type: "number";
        minimum: 1.0;
        maximum: 10.0;
        exclusiveMinimum: 1.0;
        exclusiveMaximum: 10.0;
        default: 1.0;
      };
    };
  };
};
export declare const collectionPropertyNumTest: collectionPropertyNumTestCollection & {
  item: SchemaWithId<collectionPropertyNumTestCollection["description"]>;
};
export declare type CollectionPropertyNumTest = SchemaWithId<
  typeof collectionPropertyNumTest.description
>;
export declare const extendCollectionPropertyNumTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyNumTest, TCollection>;
