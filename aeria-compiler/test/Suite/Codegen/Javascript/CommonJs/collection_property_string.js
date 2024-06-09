const {
  extendCollection,
  defineCollection,
} = require("aeria");
exports.user = defineCollection({
  description: { $id: "user", properties: { name: { type: "string" } } },
});
exports.extendUserCollection = (collection) =>
extendCollection(user, collection);
