import { extendCollection, defineCollection } from "aeria";

export const collectionPropertyConstTest = defineCollection({
  description: {
    $id: "collectionPropertyConstTest",
    properties: {
      prop1: { const: "test" },
      prop2: { const: 1.1 },
      prop3: { const: 1.0 },
      prop4: { const: false },
      prop5: { const: undefined },
      prop6: { const: null },
    },
  },
});
export const extendCollectionPropertyConstTestCollection = (collection) =>
  extendCollection(collectionPropertyConstTest, collection);
