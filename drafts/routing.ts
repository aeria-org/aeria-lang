// @ts-nocheck
import { makeRouter, defineContract } from 'aeria'

const UserAddContract = defineContract([
  {
    properties: {
      name: {
        type: 'string'
      },
      age: {
        type: 'number'
      }
    }
  },
  {
    properties: {
      success: {
        type: 'boolean'
      }
    }
  }
])

const userRouter = makeRouter()
userRouter.POST('/add', () => null, {
  contract: UserAddContract
})

export const router = makeRouter()
router.GET('/hello-world', () => null)
router.group('/user', userRouter)
