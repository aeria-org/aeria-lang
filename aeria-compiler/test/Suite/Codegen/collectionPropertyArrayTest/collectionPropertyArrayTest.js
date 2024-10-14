const { extendCollection, defineCollection } = require("aeria");

const collectionPropertyArrayTest = defineCollection({
  description: {
    $id: "collectionPropertyArrayTest",
    properties: {
      prop1: {
        type: "array",
        items: { type: "string", minLength: 10.0, maxLength: 1.0 },
        minItems: 1.0,
        maxItems: 10.0,
        uniqueItems: true,
        default: ["value1", "value2", "value3"],
      },
      prop2: {
        type: "array",
        items: {
          type: "object",
          properties: { prop3: { type: "string" }, prop4: { type: "string" } },
        },
      },
      prop4: { type: "array", items: { $ref: "file" } },
    },
  },
});
exports.collectionPropertyArrayTest = collectionPropertyArrayTest;
exports.extendcollectionPropertyArrayTestCollection = (collection) =>
  extendCollection(collectionPropertyArrayTest, collection);
