import {
  extendCollection,
  defineCollection,
} from "aeria";
export const collectionpropertystrtest = defineCollection({
  description: { $id: "collectionpropertystrtest", properties: { prop1: { type: "string" } } },
});
export const extendCollectionpropertystrtestCollection = (collection) =>
  extendCollection(collectionpropertystrtest, collection);
