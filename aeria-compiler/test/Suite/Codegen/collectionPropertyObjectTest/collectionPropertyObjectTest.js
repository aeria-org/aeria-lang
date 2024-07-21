const { extendCollection, defineCollection } = require("aeria");

exports.collectionPropertyObjectTest = defineCollection({
  description: {
    $id: "collectionPropertyObjectTest",
    properties: {
      prop1: {
        type: "object",
        required: ["prop2", "prop3"],
        properties: {
          prop2: { type: "string" },
          prop3: { type: "number" },
          prop4: {
            type: "object",
            required: ["prop5"],
            properties: {
              prop5: { type: "string" },
              prop6: { type: "string" },
            },
          },
        },
      },
    },
  },
});
exports.extendCollectionPropertyObjectTestCollection = (collection) =>
  extendCollection(collectionPropertyObjectTest, collection);
