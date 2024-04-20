import { init, createRouter } from 'aeria'
import { person } from 'aeria-runtime/collections/person'
import { pet } from 'aeria-runtime/collections/pet'

export const router = createRouter()

router.GET('/test', async (context) => {
  const person = await context.collections.person.model.findOne()
  if( person ) {
    //
  }
})

export default init({
  collections: {
    person,
    // pet,
  }
})
