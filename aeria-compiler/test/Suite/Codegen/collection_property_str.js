const {extendCollection, defineCollection} = require("aeria");
exports.collectionPropertyStrTest = defineCollection({
    description: {
        $id: "collectionPropertyStrTest",
        properties: { prop1: { type: "string" } }
    }
});
exports.extendCollectionPropertyStrTestCollection = collection => extendCollection(collectionPropertyStrTest, collection);
