const { extendCollection, defineCollection } = require("aeria");
exports.collectionPropertyNumTest = defineCollection({
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
exports.extendCollectionPropertyNumTestCollection = (collection) =>
  extendCollection(collectionPropertyNumTest, collection);
