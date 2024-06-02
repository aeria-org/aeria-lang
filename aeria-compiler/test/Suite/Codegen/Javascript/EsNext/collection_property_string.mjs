import {
  extendCollection as originalExtendCollection,
  defineCollection,
} from "aeria";
export const user = defineCollection({
  description: { $id: "user", properties: { name: { type: "string" } } },
});
export const extendCollection = (collection) =>
  originalExtendCollection(user, collection);
