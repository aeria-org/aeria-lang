import {defineCollection,deepMerge,} from "sonata-api"


export const User = {
  description:{$id:"User",properties:{username:{type:"string",},password:{type:"string",},profile:{$ref:"Profile",indexes:["first_name","last_name",],},},},
}

export const defineCollection = (collection,) => defineCollection(deepMerge(User, collection,),)

