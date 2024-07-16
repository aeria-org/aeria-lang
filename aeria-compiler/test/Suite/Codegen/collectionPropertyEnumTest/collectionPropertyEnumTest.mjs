import { extendCollection, defineCollection } from "aeria";

export const collectionPropertyEnumTest = defineCollection({
  description: {
    $id: "collectionPropertyEnumTest",
    properties: { prop1: { enum: ["value1", "value2", "value3"] } },
  },
});
export const extendCollectionPropertyEnumTestCollection = (collection) =>
  extendCollection(collectionPropertyEnumTest, collection);
