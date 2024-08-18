import assert from 'assert'
import { test, expect } from 'vitest'
import {
  isLeft,
  unwrap,
  getDeclarations,
  generateRootPackageJson,
  generateCollectionsIndexJs,
  generateCollectionsIndexDts,
  generateScaffolding,
  writeBaseFiles,
} from '../src/index.js'

import {
  buildOptions,
  validResultEither,
  invalidResultEither,
} from './fixtures/build.js'

test('compileSources to esnext successfully', () => {
  assert(!isLeft(validResultEither))
})

test('detects error on invalid source', () => {
  assert(isLeft(invalidResultEither))
})

test('extracts declarations right', () => {
  assert(!isLeft(validResultEither))
  const result = unwrap(validResultEither)
  const declarations = getDeclarations(result)
  expect(declarations.length).toBe(2)
  expect(declarations[0].name).toBe('pet')
  expect(declarations[0].type).toBe('collection')
  expect(declarations[1].name).toBe('person')
  expect(declarations[1].type).toBe('collection')
})

test('generates the root package.json right (esm)', () => {
  const json = generateRootPackageJson(buildOptions)

  expect(JSON.stringify(json)).toBe(JSON.stringify({
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
test('generates the root package.json right (cjs)', () => {
  const json = generateRootPackageJson({
    ...buildOptions,
    module: 'commonjs',
  })
  expect(json.main).toBe('index.js')
})

test('generates the root index.js right', () => {
  assert(!isLeft(validResultEither))
  const result = unwrap(validResultEither)
  const source = generateCollectionsIndexJs(getDeclarations(result), buildOptions)

  console.log(source)
})

test('generates the root index.d.ts right', () => {
  assert(!isLeft(validResultEither))
  const result = unwrap(validResultEither)
  const source = generateCollectionsIndexDts(getDeclarations(result), buildOptions)

  console.log(source)
})

test('generates proper scaffolding', async () => {
  const scaffolding = await generateScaffolding(buildOptions)
  expect(scaffolding[0]).toBe('node_modules/.test/collections')
})

test('emits scaffolding files (esm)', async () => {
  assert(!isLeft(validResultEither))
  const result = unwrap(validResultEither)
  const declarations = getDeclarations(result)

  const files = Object.keys(await writeBaseFiles(declarations, buildOptions))

  expect(files[0]).toBe('node_modules/.test/package.json')
  expect(files[1]).toBe('node_modules/.test/index.mjs')
  expect(files[2]).toBe('node_modules/.test/index.d.mts')
  expect(files[3]).toBe('node_modules/.test/collections/index.mjs')
  expect(files[4]).toBe('node_modules/.test/collections/index.d.mts')
})

test('emits scaffolding files (cjs)', async () => {
  assert(!isLeft(validResultEither))
  const result = unwrap(validResultEither)
  const declarations = getDeclarations(result)

  const files = Object.keys(await writeBaseFiles(declarations, {
    ...buildOptions,
    module: 'commonjs',
  }))

  expect(files[1]).toBe('node_modules/.test/index.js')
  expect(files[2]).toBe('node_modules/.test/index.d.ts')
  expect(files[3]).toBe('node_modules/.test/collections/index.js')
  expect(files[4]).toBe('node_modules/.test/collections/index.d.ts')
})

