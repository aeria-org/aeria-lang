import { init, createRouter } from 'aeria'
export * from 'aeria-runtime/collections/pet'
import { person } from 'aeria-runtime/collections/person'
import { pet } from 'aeria-runtime/collections/pet'

export const router = createRouter()

export const collections = {
  person,
  pet,
}

router.GET('/test', async (context) => {
  const person = await context.collections.person.model.findOne()
  const pet = await context.collections.pet.model.findOne()

  if( person ) {
    person.options.friends[0].type
  }


  if( pet ) {
    pet.breed === 'dog'
    // @ts-expect-error
    pet.breed === 'lizard'
  }
})

export default init()

