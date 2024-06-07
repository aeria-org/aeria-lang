import assert from 'assert'
import { compile, isLeft, unwrapEither, getCollectionNames } from '../src'

const validSource = `
collection Pet {
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

const invalidSource = `
collection Pet {
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
  filename: 'dummy.aeria',
  source: validSource,
  module: 'esnext'
})

const invalidResultEither = compile({
  filename: 'dummy.aeria',
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
    const collectionNames = getCollectionNames(result)
    assert(collectionNames.length === 2)
    assert(collectionNames[0] === 'pet')
    assert(collectionNames[1] === 'person')
  })
})

