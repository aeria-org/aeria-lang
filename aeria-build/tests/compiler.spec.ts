import assert from 'assert'
import {
  compile,
  isLeft,
  unwrapEither,
  getDeclarations,
  generateRootPackageJson,
  generateRootIndexJs,
  generateRootIndexDts,
} from '../src'

const DUMMY_FILENAME = 'dummy.aeria'

const validSource =
 `collection Pet {
  properties {
    name str
    breed enum @values(["dog", "cat"])
  }
}

collection Person {
  properties {
    name str
    age int
    pets []Pet
  }
}
`

const invalidSource = 
`collection Pet {
  properties {
    name str
    breed enum @values(["dog", "cat"])
  }
}

collection Person {
  properties {
    name str
    age int
    pets []PetXX
  }
}
`

const validResultEither = compile({
  filename: DUMMY_FILENAME,
  source: validSource,
  module: 'esnext'
})

const invalidResultEither = compile({
  filename: DUMMY_FILENAME,
  source: invalidSource,
  module: 'esnext'
})

describe('Compile', () => {
  it('compiles to esnext successfully', () => {
    assert(!isLeft(validResultEither))
  })
  it('detects error on invalid source', () => {
    assert(isLeft(invalidResultEither))
  })
  it('extracts collection names right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const collectionNames = getDeclarations(result)
    assert(collectionNames.length === 2)
    assert(collectionNames[0] === 'pet')
    assert(collectionNames[1] === 'person')
  })
  it('generates the root package.json right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const json = generateRootPackageJson(result)

    assert(JSON.stringify(json) === JSON.stringify({
      "exports": {
        "pet": {
          "require": "./pet.js",
          "import": "./pet.mjs",
          "types": "./pet.d.ts"
        },
        "person": {
          "require": "./person.js",
          "import": "./person.mjs",
          "types": "./person.d.ts"
        }
      }
    }))
  })
  it('generates the root index.js right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateRootIndexJs(result)

    console.log(source)
  })
  it('generates the root index.d.ts right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateRootIndexDts(result)

    console.log(source)
  })
})

