const { extendCollection, defineCollection } = require("aeria");

exports.collectionPropertyObjectTest = defineCollection({
  description: {
    $id: "collectionPropertyObjectTest",
    properties: {
      prop1: {
        required: ["prop2", "prop3"],
        type: "object",
        properties: {
          prop2: { type: "string" },
          prop3: { type: "number" },
          prop4: {
            required: ["prop5"],
            type: "object",
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
