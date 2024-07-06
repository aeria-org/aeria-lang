const {
  extendCollection,
  defineCollection,
} = require("aeria");

exports.collectionpropertystrtest = defineCollection({
  description: { $id: "collectionpropertystrtest", properties: { prop1: { type: "string" } } },
});

exports.extendCollectionpropertystrtestCollection = (collection) => extendCollection(collectionpropertystrtest, collection)
