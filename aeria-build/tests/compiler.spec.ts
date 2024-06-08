import type { BuildOptions } from '../src/index.js'
import assert from 'assert'
import {
  compile,
  isLeft,
  unwrapEither,
  getDeclarations,
  generateRootPackageJson,
  generateCollectionsIndexJs,
  generateCollectionsIndexDts,
  generateScaffolding,
  writeBaseFiles,

} from '../src/index.js'

const DUMMY_FILENAME = 'dummy.aeria'

const buildOptions: BuildOptions = {
  outDir: 'node_modules/.test',
  module: 'esnext',
  dryRun: true,
}

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

  it('generates the root package.json right (esm)', () => {
    const json = generateRootPackageJson(buildOptions)

    assert(JSON.stringify(json) === JSON.stringify({
      "main": "index.mjs",
      "types": "index.d.ts",
      "exports": {
        "./collections": {
          "require": "./collections/index.js",
          "import": "./collections/index.mjs",
          "types": "./collections/index.d.ts"
        }
      }
    }))
  })
  it('generates the root package.json right (cjs)', () => {
    const json = generateRootPackageJson({
      ...buildOptions,
      module: 'commonjs',
    })
    assert(json.main === 'index.js')
  })

  it('generates the root index.js right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateCollectionsIndexJs(getDeclarations(result), buildOptions)

    console.log(source)
  })
  it('generates the root index.d.ts right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const source = generateCollectionsIndexDts(getDeclarations(result), buildOptions)

    console.log(source)
  })
  it('generates proper scaffolding', async () => {
    const scaffolding = await generateScaffolding(buildOptions)
    assert(scaffolding[0] === 'node_modules/.test/collections')
  })
  it('emits scaffolding files (esm)', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const declarations = getDeclarations(result)

    const files = Object.keys(await writeBaseFiles(declarations, buildOptions))

    assert(files[0] === 'node_modules/.test/package.json')
    assert(files[1] === 'node_modules/.test/collections/index.mjs')
    assert(files[2] === 'node_modules/.test/collections/index.d.ts')
  })
  it('emits scaffolding files (cjs)', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrapEither(validResultEither)
    const declarations = getDeclarations(result)

    const files = Object.keys(await writeBaseFiles(declarations, {
      ...buildOptions,
      module: 'commonjs',
    }))
    assert(files[1] === 'node_modules/.test/collections/index.js')
  })
})

