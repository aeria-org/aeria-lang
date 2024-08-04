const { extendCollection, defineCollection, func1, func2 } = require("aeria");
exports.collectionSecutiryTest = defineCollection({
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
exports.extendCollectionSecutiryTestCollection = (collection) =>
  extendCollection(collectionSecutiryTest, collection);
