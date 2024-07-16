import { extendCollection, defineCollection } from "aeria";

export const collectionGetters = defineCollection({
  description: {
    $id: "collectionGetters",
    properties: {
      prop1: { type: "integer" },
      prop2: { type: "integer" },
      sum: {
        getter: (doc) => {
          return doc.prop1 + doc.prop2;
        },
      },
    },
  },
});
export const extendCollectionGettersCollection = (collection) =>
  extendCollection(collectionGetters, collection);
