import {
  extendCollection,
  defineCollection
} from "aeria";
export const collectionPropertyStrTest = defineCollection({
  description: {
      $id: "collectionPropertyStrTest",
      properties: { prop1: { type: "string" } }
  }
});
export const extendCollectionPropertyStrTestCollection = collection => extendCollection(collectionPropertyStrTest, collection);
