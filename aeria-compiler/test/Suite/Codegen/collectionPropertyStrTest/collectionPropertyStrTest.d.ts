import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

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
export declare type CollectionPropertyStrTest = SchemaWithId<
  typeof collectionPropertyStrTest.description
>;
export declare const extendCollectionPropertyStrTestCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPropertyStrTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: Pick<TCollection, keyof Collection>
) => ExtendCollection<typeof collectionPropertyStrTest, TCollection>;
