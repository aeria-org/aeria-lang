// @ts-nocheck
import { defineCollection } from "aeria";

export const person = defineCollection({
  description: {
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
        indexes: [
          "first_name",
          "last_name"
        ],
      },
      friends: {
        type: "array",
        items: {
          $ref: "person",
          indexes: [
            "first_name",
            "last_name"
          ],
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
    table: [
      "first_name",
      "last_name",
      "age"
    ],
  },
})
