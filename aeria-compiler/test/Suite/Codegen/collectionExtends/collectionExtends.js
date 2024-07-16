const { extendCollection, defineCollection, register } = require("aeria");
const { users: original } = require("aeria");
exports.collectionExtends = extendCollection(original, {
  description: {
    $id: "collectionExtends",
    properties: { prop1: { type: "string" } },
  },
  functions: { register },
  exposedFunctions: { register: true },
});
exports.extendCollectionExtendsCollection = (collection) =>
  extendCollection(collectionExtends, collection);
