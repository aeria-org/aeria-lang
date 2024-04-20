import { init, createRouter } from 'aeria'
import { person } from 'aeria-runtime/person'
// import { pet } from 'aeria-runtime/pet'

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
