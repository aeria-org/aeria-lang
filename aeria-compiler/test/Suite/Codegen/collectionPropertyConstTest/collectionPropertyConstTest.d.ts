import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

export declare type collectionPropertyConstTestCollection = {
  description: {
    $id: "collectionPropertyConstTest";
    properties: {
      prop1: { const: "test" };
      prop2: { const: 1.1 };
      prop3: { const: 1.0 };
      prop4: { const: false };
      prop5: { const: undefined };
      prop6: { const: null };
    };
  };
};
export declare const collectionPropertyConstTest: collectionPropertyConstTestCollection & {
  item: SchemaWithId<collectionPropertyConstTestCollection["description"]>;
};
export declare type CollectionPropertyConstTest = SchemaWithId<
  typeof collectionPropertyConstTest.description
>;
export declare const extendCollectionPropertyConstTestCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPropertyConstTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyConstTest, TCollection>;
