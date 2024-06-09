import {
  extendCollection,
  defineCollection,
} from "aeria";
export const user = defineCollection({
  description: { $id: "user", properties: { name: { type: "string" } } },
});
export const extendUserCollection = (collection) =>
  extendCollection(user, collection);
