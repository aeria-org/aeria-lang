import { users as original } from "aeria";
import { extendCollection, defineCollection, register } from "aeria";
export const collectionExtends = extendCollection(original, {
  description: {
    $id: "collectionExtends",
    properties: { prop1: { type: "string" } },
  },
  functions: { register },
  exposedFunctions: { register: true },
});
export const extendCollectionExtendsCollection = (collection) =>
  extendCollection(collectionExtends, collection);
