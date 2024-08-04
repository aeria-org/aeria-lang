import {
  Collection,
  SchemaWithId,
  ExtendCollection,
  func1,
  func2
} from "aeria";
export declare type collectionSecutiryTestCollection = {
  description: { $id: "collectionSecutiryTest"; properties: {} };
  functions: { func1: typeof func1; func2: typeof func2 };
  security: {
    functions: {
      func1: {
        rateLimiting: { strategy: "ip"; scale: 10.0 };
        logging: { strategy: "tenant" };
      };
      func2: { logging: { strategy: "tenant" } };
    };
  };
};
export declare const collectionSecutiryTest: collectionSecutiryTestCollection & {
  item: SchemaWithId<collectionSecutiryTestCollection["description"]>;
};
export declare type CollectionSecutiryTest = SchemaWithId<
  typeof collectionSecutiryTest.description
>;
export declare const extendCollectionSecutiryTestCollection: <
  const TCollection extends { [P in keyof Collection]?: Partial<Collection[P]> }
>(
  collection: TCollection
) => ExtendCollection<typeof collectionSecutiryTest, TCollection>;
