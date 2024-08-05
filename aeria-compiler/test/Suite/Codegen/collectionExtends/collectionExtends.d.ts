import { users as original } from "aeria";
import { Collection, SchemaWithId, ExtendCollection, Context, register } from "aeria";
export declare type collectionExtendsCollection = ExtendCollection<
  typeof original,
  {
    description: {
      $id: "collectionExtends";
      properties: { prop1: { type: "string" } };
    };
    functions: { register: typeof register };
    exposedFunctions: { register: true };
  }
>;
export declare const collectionExtends: collectionExtendsCollection & {
  item: SchemaWithId<collectionExtendsCollection["description"]>;
};
export declare type CollectionExtends = SchemaWithId<
  typeof collectionExtends.description
>;
export declare const extendCollectionExtendsCollection: <
  const TCollection extends {
    [P in Exclude<keyof Collection, "functions">]?: Partial<Collection[P]>;
  } & {
    functions?: {
      [F: string]: (
        payload: any,
        context: Context<typeof collectionExtends["description"]>
      ) => unknown;
    };
  }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionExtends, TCollection>;
