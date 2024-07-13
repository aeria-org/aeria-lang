import { extendCollection, defineCollection } from "aeria";

export const collectionPropertyStrTest = defineCollection({
  description: {
    $id: "collectionPropertyStrTest",
    properties: {
      prop1: {
        type: "string",
        minLength: 1.0,
        maxLength: 1.0,
        format: "date",
        mask: "yy-mm-aaaa",
        inputType: "text",
        default: "12-03-2024",
      },
    },
  },
});
export const extendcollectionPropertyStrTestCollection = (collection) =>
  extendCollection(collectionPropertyStrTest, collection);
