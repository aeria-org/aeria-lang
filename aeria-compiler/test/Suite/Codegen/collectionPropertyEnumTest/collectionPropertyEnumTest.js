const { extendCollection, defineCollection } = require("aeria");

exports.collectionPropertyEnumTest = defineCollection({
  description: {
    $id: "collectionPropertyEnumTest",
    properties: { prop1: { enum: ["value1", "value2", "value3"] } },
  },
});
exports.extendCollectionPropertyEnumTestCollection = (collection) =>
  extendCollection(collectionPropertyEnumTest, collection);
