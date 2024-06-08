import assert from 'assert'
import {
  compile,
  isLeft,
  unwrapEither,
  getDeclarations,
  generateRootPackageJson,
  generateRootIndexJs,
  generateRootIndexDts,
  generateScaffolding,
  writeFiles,

} from '../src/index.js'

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
  it('extracts declarations right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const declarations = getDeclarations(result)
    assert(declarations.length === 2)
    assert(declarations[0].name === 'pet')
    assert(declarations[0].type === 'collection')
    assert(declarations[1].name === 'person')
    assert(declarations[1].type === 'collection')
  })
  it('generates the root package.json right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const json = generateRootPackageJson(getDeclarations(result))

    assert(JSON.stringify(json) === JSON.stringify({
      "exports": {
        "./collections/pet": {
          "require": "./collections/pet.js",
          "import": "./collections/pet.mjs",
          "types": "./collections/pet.d.ts"
        },
        "./collections/person": {
          "require": "./collections/person.js",
          "import": "./collections/person.mjs",
          "types": "./collections/person.d.ts"
        }
      }
    }))
  })
  it('generates the root index.js right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateRootIndexJs(getDeclarations(result))

    console.log(source)
  })
  it('generates the root index.d.ts right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateRootIndexDts(getDeclarations(result))

    console.log(source)
  })
  it('generates proper scaffolding', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const declarations = getDeclarations(result)

    const scaffolding = await generateScaffolding(declarations, {
      outDir: 'node_modules/.test',
      dryRun: true,
    })

    assert(scaffolding[0] === 'node_modules/.test/collections/pet')
    assert(scaffolding[1] === 'node_modules/.test/collections/person')
  })
  it('emits scaffolding files', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const declarations = getDeclarations(result)

    const files = Object.keys(await writeFiles(declarations, {
      outDir: 'node_modules/.test',
      dryRun: true,
    }))

    assert(files[0] === 'node_modules/.test/package.json')
    assert(files[1] === 'node_modules/.test/index.js')
    assert(files[2] === 'node_modules/.test/index.d.ts')
  })
})

