import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

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
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPropertyEnumTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: Pick<TCollection, keyof Collection>
) => ExtendCollection<typeof collectionPropertyEnumTest, TCollection>;
