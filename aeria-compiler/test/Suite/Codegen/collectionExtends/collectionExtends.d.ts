import { users as original } from "aeria";
import { Collection, SchemaWithId, ExtendCollection, register } from "aeria";
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
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionExtends, TCollection>;
