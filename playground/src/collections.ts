export * as collections from '../.aeria/out/collections/index.mjs'
import { extendPetCollection } from '../.aeria/out/collections/pet.mjs'

export const pet = extendPetCollection({
  description: {
    properties: {
      is_docile: {
        type: 'boolean',
      },
    },
  },
})

