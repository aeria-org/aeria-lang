const { extendCollection, defineCollection } = require("aeria");

exports.collectionPropertyStrTest = defineCollection({
  description: {
    $id: "collectionPropertyStrTest",
    properties: {
      prop1: {
        type: "string",
        minLength: 1.0,
        maxLength: 1.0,
        format: "date",
        mask: "yy-mm-aaaa",
        inputType: "text",
        default: "12-03-2024",
      },
    },
  },
});
exports.extendCollectionPropertyStrTestCollection = (collection) =>
  extendCollection(collectionPropertyStrTest, collection);
