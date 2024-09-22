import { Collection,SchemaWithId,ExtendCollection,Context } from "aeria"
export declare type collectionRequiredTestCollection = {description: {$id: "collectionRequiredTest",properties: {comment: {type: "string"},ticket: {type: "string"}},formLayout: {fields: {comment: {if: {operator: "truthy",term1: {and: ["_id",{operator: "eq",term1: "ticket",term2: "1"}]}}}}}}}
export declare const collectionRequiredTest: collectionRequiredTestCollection & {item: SchemaWithId<collectionRequiredTestCollection["description"]>}
export declare type CollectionRequiredTest = SchemaWithId<typeof collectionRequiredTest.description>
export declare const extendCollectionRequiredTestCollection: <
          const TCollection extends {
            [P in Exclude<keyof Collection, "functions">] ? : Partial <Collection[P]>
          } &{
            functions?: {
              [F: string]: (payload: any, context: Context<typeof collectionRequiredTest["description"]>) => unknown
            }
          }>(collection: TCollection) => ExtendCollection<typeof collectionRequiredTest,TCollection>
