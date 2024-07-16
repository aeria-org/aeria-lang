import { extendCollection, defineCollection } from "aeria";
export const collectionPropertyNumTest = defineCollection({
  description: {
    $id: "collectionPropertyNumTest",
    properties: {
      prop1: {
        type: "number",
        minimum: 1.0,
        maximum: 10.0,
        exclusiveMinimum: 1.0,
        exclusiveMaximum: 10.0,
        default: 1.0,
      },
    },
  },
});
export const extendCollectionPropertyNumTestCollection = (collection) =>
  extendCollection(collectionPropertyNumTest, collection);
