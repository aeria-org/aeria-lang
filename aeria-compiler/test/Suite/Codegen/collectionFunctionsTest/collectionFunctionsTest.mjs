import { extendCollection, defineCollection, get, insert, remove } from "aeria";

export const collectionFunctionsTest = defineCollection({
  description: { $id: "collectionFunctionsTest", properties: {} },
  functions: { get, insert, remove },
  exposedFunctions: {
    customFunc2: true,
    get: "unauthenticated",
    insert: ["root"],
  },
});
export const extendCollectionFunctionsTestCollection = (collection) =>
  extendCollection(collectionFunctionsTest, collection);
