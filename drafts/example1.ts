// @ts-nocheck
import { defineDescription, defineCollection } from "aeria";

export const description = defineDescription({
  $id: "person",
  properties: {
    first_name: {
      type: "string",
    },
    last_name: {
      type: "string",
    },
    age: {
      type: "number",
    },
    responsible: {
      $ref: "person",
      indexes: ["first_name", "last_name"],
    },
    friends: {
      type: "array",
      items: {
        $ref: "person",
        indexes: ["first_name", "last_name"],
        constrants: {
          not: {
            operator: "equal",
            term1: "blocked",
            term2: false,
          },
        },
      },
    },
  },
  table: ["first_name", "last_name", "age"],
});

export const person = defineCollection(() => ({
  description,
}));
