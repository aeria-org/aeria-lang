import { Collection, SchemaWithId, ExtendCollection } from "aeria";

export declare type collectionPropertyObjectTestCollection = {
  description: {
    $id: "collectionPropertyObjectTest";
    properties: {
      prop1: {
        type: "object";
        required: ["prop2", "prop3"];
        properties: {
          prop2: { type: "string" };
          prop3: { type: "number" };
          prop4: {
            type: "object";
            required: ["prop5"];
            properties: {
              prop5: { type: "string" };
              prop6: { type: "string" };
            };
          };
        };
      };
    };
  };
};
export declare const collectionPropertyObjectTest: collectionPropertyObjectTestCollection & {
  item: SchemaWithId<collectionPropertyObjectTestCollection["description"]>;
};
export declare type CollectionPropertyObjectTest = SchemaWithId<
  typeof collectionPropertyObjectTest.description
>;
export declare const extendCollectionPropertyObjectTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyObjectTest, TCollection>;
