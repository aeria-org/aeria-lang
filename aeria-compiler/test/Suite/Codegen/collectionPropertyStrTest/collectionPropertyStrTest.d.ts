import { Collection, SchemaWithId, ExtendCollection } from "aeria";

export declare type collectionPropertyStrTestCollection = {
  description: {
    $id: "collectionPropertyStrTest";
    properties: {
      prop1: {
        type: "string";
        minLength: 1.0;
        maxLength: 1.0;
        format: "date";
        mask: "yy-mm-aaaa";
        inputType: "text";
        default: "12-03-2024";
      };
    };
  };
};
export declare const collectionPropertyStrTest: collectionPropertyStrTestCollection & {
  item: SchemaWithId<collectionPropertyStrTestCollection["description"]>;
};
export declare type collectionPropertyStrTest = SchemaWithId<
  typeof collectionPropertyStrTest.description
>;
export declare const extendcollectionPropertyStrTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyStrTest, TCollection>;
