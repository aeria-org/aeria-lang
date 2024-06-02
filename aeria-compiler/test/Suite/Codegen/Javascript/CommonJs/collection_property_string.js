const {
  extendCollection: originalExtendCollection,
  defineCollection,
} = require("aeria");
exports.user = defineCollection({
  description: { $id: "user", properties: { name: { type: "string" } } },
});
exports.extendCollection = (collection) =>
  originalExtendCollection(user, collection);
