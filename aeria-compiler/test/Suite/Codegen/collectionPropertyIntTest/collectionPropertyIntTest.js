const { extendCollection, defineCollection } = require("aeria");
const collectionPropertyIntTest = defineCollection({
  description: {
    $id: "collectionPropertyIntTest",
    properties: {
      prop1: {
        type: "integer",
        minimum: 1.0,
        maximum: 10.0,
        exclusiveMinimum: 1.0,
        exclusiveMaximum: 10.0,
        default: 1.0,
      },
    },
  },
});
exports.collectionPropertyIntTest = collectionPropertyIntTest;
exports.extendCollectionPropertyIntTestCollection = (collection) =>
  extendCollection(collectionPropertyIntTest, collection);
