const { extendCollection, defineCollection } = require("aeria");

const collectionGetters = defineCollection({
  description: {
    $id: "collectionGetters",
    properties: {
      prop1: { type: "integer" },
      prop2: { type: "integer" },
      sum: {
        getter: (doc) => {
          return doc.prop1 + doc.prop2;
        },
      },
    },
  },
});
exports.collectionGetters = collectionGetters;
exports.extendCollectionGettersCollection = (collection) =>
  extendCollection(collectionGetters, collection);
