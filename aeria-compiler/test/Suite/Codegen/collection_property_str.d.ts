import { Collection, SchemaWithId, ExtendCollection } from "aeria";
export declare type collectionPropertyStrTestCollection = {
    description: {
        $id: "collectionPropertyStrTest";
        properties: {
            prop1: {
                type: "string";
            };
        };
    };
};
export declare const collectionPropertyStrTest: collectionPropertyStrTestCollection & {
    item: SchemaWithId<collectionPropertyStrTestCollection["description"]>;
};
export declare type CollectionPropertyStrTest = SchemaWithId<typeof collectionPropertyStrTest.description>;
export declare const extendCollectionPropertyStrTestCollection: <const TCollection extends {
    [P in keyof Collection]?: Partial<Collection[P]>;
}>(collection: TCollection) => ExtendCollection<typeof collectionPropertyStrTest, TCollection>;
