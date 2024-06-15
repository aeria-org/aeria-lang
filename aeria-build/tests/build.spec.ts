import type { BuildOptions } from '../src/index.js'
import assert from 'assert'
import {
  compileSource,
  isLeft,
  unwrap,
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

const validResultEither = compileSource({
  filename: DUMMY_FILENAME,
  source: validSource,
  module: 'esnext'
})

const invalidResultEither = compileSource({
  filename: DUMMY_FILENAME,
  source: invalidSource,
  module: 'esnext'
})

describe('Build', () => {
  it('compileSources to esnext successfully', () => {
    assert(!isLeft(validResultEither))
  })
  it('detects error on invalid source', () => {
    assert(isLeft(invalidResultEither))
  })
  it('extracts declarations right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrap(validResultEither)
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
      main: 'index.mjs',
      types: 'index.d.mts',
      exports: {
        '.': {
          'require': './index.js',
          'import': './index.mjs',
          'types': './index.d.ts'
        },
        './collections': {
          'require': './collections/index.js',
          'import': './collections/index.mjs',
          'types': './collections/index.d.ts'
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
    const result = unwrap(validResultEither)
    const source = generateCollectionsIndexJs(getDeclarations(result), buildOptions)

    console.log(source)
  })
  it('generates the root index.d.ts right', () => {
    assert(!isLeft(validResultEither))
    const result = unwrap(validResultEither)
    const source = generateCollectionsIndexDts(getDeclarations(result), buildOptions)

    console.log(source)
  })
  it('generates proper scaffolding', async () => {
    const scaffolding = await generateScaffolding(buildOptions)
    assert(scaffolding[0] === 'node_modules/.test/collections')
  })
  it('emits scaffolding files (esm)', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrap(validResultEither)
    const declarations = getDeclarations(result)

    const files = Object.keys(await writeBaseFiles(declarations, buildOptions))

    assert(files[0] === 'node_modules/.test/package.json')
    assert(files[1] === 'node_modules/.test/index.mjs')
    assert(files[2] === 'node_modules/.test/index.d.mts')
    assert(files[3] === 'node_modules/.test/collections/index.mjs')
    assert(files[4] === 'node_modules/.test/collections/index.d.mts')
  })
  it('emits scaffolding files (cjs)', async () => {
    assert(!isLeft(validResultEither))
    const result = unwrap(validResultEither)
    const declarations = getDeclarations(result)

    const files = Object.keys(await writeBaseFiles(declarations, {
      ...buildOptions,
      module: 'commonjs',
    }))

    console.log(files)
    assert(files[1] === 'node_modules/.test/index.js')
    assert(files[2] === 'node_modules/.test/index.d.ts')
    assert(files[3] === 'node_modules/.test/collections/index.js')
    assert(files[4] === 'node_modules/.test/collections/index.d.ts')
  })
})

