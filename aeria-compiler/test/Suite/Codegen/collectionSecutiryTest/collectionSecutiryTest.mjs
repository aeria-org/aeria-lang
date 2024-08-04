import { extendCollection, defineCollection, func1, func2 } from "aeria";
export const collectionSecutiryTest = defineCollection({
  description: { $id: "collectionSecutiryTest", properties: {} },
  functions: { func1, func2 },
  security: {
    functions: {
      func1: {
        rateLimiting: { strategy: "ip", scale: 10.0 },
        logging: { strategy: "tenant" },
      },
      func2: { logging: { strategy: "tenant" } },
    },
  },
});
export const extendCollectionSecutiryTestCollection = (collection) =>
  extendCollection(collectionSecutiryTest, collection);
