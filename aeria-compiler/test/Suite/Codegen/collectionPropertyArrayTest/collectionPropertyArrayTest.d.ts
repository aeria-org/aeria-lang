import { Collection, SchemaWithId, ExtendCollection, Context } from "aeria";

export declare type collectionPropertyArrayTestCollection = {
  description: {
    $id: "collectionPropertyArrayTest";
    properties: {
      prop1: {
        type: "array";
        items: { type: "string"; minLength: 10.0; maxLength: 1.0 };
        minItems: 1.0;
        maxItems: 10.0;
        uniqueItems: true;
        default: ["value1", "value2", "value3"];
      };
      prop2: {
        type: "array";
        items: {
          type: "object";
          properties: { prop3: { type: "string" }; prop4: { type: "string" } };
        };
      };
      prop4: { type: "array"; items: { $ref: "file" } };
    };
  };
};
export declare const collectionPropertyArrayTest: collectionPropertyArrayTestCollection & {
  item: SchemaWithId<collectionPropertyArrayTestCollection["description"]>;
};
export declare type collectionPropertyArrayTest = SchemaWithId<
  typeof collectionPropertyArrayTest.description
>;
export declare const extendcollectionPropertyArrayTestCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionPropertyArrayTest["description"]>
      ) => unknown;
    };
  }
>(
  collection: Pick<TCollection, keyof Collection>
) => ExtendCollection<typeof collectionPropertyArrayTest, TCollection>;
