const {defineCollection,deepMerge,} = require("sonata-api")
exports.User = (description,) => defineCollection(deepMerge({description:{$id:"User",properties:{username:{type:"string",},password:{type:"string",},profile:{$ref:"Profile",indexes:["first_name","last_name",],},},},},description,),)
