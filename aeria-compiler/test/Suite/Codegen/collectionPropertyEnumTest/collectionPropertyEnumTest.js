const { extendCollection, defineCollection } = require("aeria");

const collectionPropertyEnumTest = defineCollection({
  description: {
    $id: "collectionPropertyEnumTest",
    properties: { prop1: { enum: ["value1", "value2", "value3"] } },
  },
});
exports.collectionPropertyEnumTest = collectionPropertyEnumTest;
exports.extendCollectionPropertyEnumTestCollection = (collection) =>
  extendCollection(collectionPropertyEnumTest, collection);
