import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

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
export declare type CollectionPropertyIntTest = SchemaWithId<
  typeof collectionPropertyIntTest.description
>;
export declare const extendCollectionPropertyIntTestCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPropertyIntTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionPropertyIntTest, TCollection>;
