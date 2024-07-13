import { Collection, SchemaWithId, ExtendCollection } from "aeria";

export declare type collectionPropertyIntTestCollection = {
  description: {
    $id: "collectionPropertyIntTest";
    properties: {
      prop1: {
        type: "integer";
        minimum: 1.0;
        maximum: 10.0;
        exclusiveMinimum: 1.0;
        exclusiveMaximum: 10.0;
        default: 1.0;
      };
    };
  };
};
export declare const collectionPropertyIntTest: collectionPropertyIntTestCollection & {
  item: SchemaWithId<collectionPropertyIntTestCollection["description"]>;
};
export declare type collectionPropertyIntTest = SchemaWithId<
  typeof collectionPropertyIntTest.description
>;
export declare const extendcollectionPropertyIntTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyIntTest, TCollection>;
