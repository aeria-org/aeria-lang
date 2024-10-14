const {
  extendCollection,
  defineCollection,
  get,
  insert,
  remove,
} = require("aeria");

const collectionFunctionsTest = defineCollection({
  description: { $id: "collectionFunctionsTest", properties: {} },
  functions: { get, insert, remove },
  exposedFunctions: {
    customFunc2: true,
    get: "unauthenticated",
    insert: ["root"],
  },
});
exports.collectionFunctionsTest = collectionFunctionsTest;
exports.extendCollectionFunctionsTestCollection = (collection) =>
  extendCollection(collectionFunctionsTest, collection);
