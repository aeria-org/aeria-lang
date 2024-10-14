const { users: original } = require("aeria");
const { extendCollection, defineCollection, register } = require("aeria");
const collectionExtends = extendCollection(original, {
  description: {
    $id: "collectionExtends",
    properties: { prop1: { type: "string" } },
  },
  functions: { register },
  exposedFunctions: { register: true },
});
exports.collectionExtends = collectionExtends;
exports.extendCollectionExtendsCollection = (collection) =>
  extendCollection(collectionExtends, collection);
