import { init, createRouter } from 'aeria'
export * from './collections.js'

export const router = createRouter()

router.GET('/test', async (context) => {
  const { result: person } = await context.collections.person.functions.get({
    filters: {}
  })

  const pet = await context.collections.pet.model.findOne()

  if( person ) {
    person.options.friends[0].name
  }


  if( pet ) {
    pet.breed === 'dog'
    // @ts-expect-error
    pet.breed === 'lizard'
  }
})

export default init()

