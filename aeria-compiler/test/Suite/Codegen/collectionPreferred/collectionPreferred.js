const { extendCollection, defineCollection } = require("aeria");

const collectionPreferred = defineCollection({
  description: {
    $id: "collectionPreferred",
    properties: { prop1: { type: "string" }, prop2: { type: "string" } },
    preferred: { role1: { table: ["prop1"] }, role2: { table: ["prop2"] } },
  },
});
exports.collectionPreferred = collectionPreferred;
exports.extendCollectionPreferredCollection = (collection) =>
  extendCollection(collectionPreferred, collection);
