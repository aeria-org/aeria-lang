import {defineCollection,deepMerge,} from "sonata-api"
export const Profile = (description,) => defineCollection(deepMerge({description:{$id:"Profile",properties:{first_name:{type:"string",},last_name:{type:"string",},profile_picture:{$ref:"File",accept:["image/*",],},name:{getter:(values) => {
        return `${values.first_name} ${values.last_name}`;
      }
      ,},},},},description,),)
