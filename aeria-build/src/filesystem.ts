import type { CompilationResult } from '@aeria-lang/compiler'
import type { Declaration, BuildOptions } from './types.js'
import * as fs from 'fs'
import * as path from 'path'
import { glob } from 'glob'
import { compileSource, isLeft, unwrap, getDeclarations } from './core.js'
import {
  addJsExtension,
  addDtsExtension,
  getDeclarationPath,
  generateRootPackageJson,
  generateRootIndexJs,
  generateRootIndexDts,
  generateCollectionsIndexJs,
  generateCollectionsIndexDts,

} from './codegen.js'

export const generateScaffolding = async (options: BuildOptions) => {
  const directories = [path.join(options.outDir, 'collections')]

  if( !options.dryRun ) {
    for( const dir of directories ) {
      await fs.promises.mkdir(dir, {
        recursive: true,
      })
    }
  }

  return directories
}

export const writeBaseFiles = async (declarations: Declaration[], options: BuildOptions) => {
  const result: Record<string, string> = {}

  const write = async (path: string, content: string) => {
    result[path] = content
    if( !options.dryRun ) {
      return fs.promises.writeFile(path, content)
    }
  }

  await write(
    path.join(options.outDir, 'package.json'),
    JSON.stringify(generateRootPackageJson(options), null, 2),
  )
  await write(
    path.join(options.outDir, addJsExtension('index', options)),
    generateRootIndexJs(declarations, options),
  )
  await write(
    path.join(options.outDir, addDtsExtension('index', options)),
    generateRootIndexDts(declarations, options),
  )
  await write(
    path.join(options.outDir, 'collections', addJsExtension('index', options)),
    generateCollectionsIndexJs(declarations, options),
  )
  await write(
    path.join(options.outDir, 'collections', addDtsExtension('index', options)),
    generateCollectionsIndexDts(declarations, options),
  )

  return result
}

export const build = async (patterns: string[], options: BuildOptions) => {
  const compilationResults: CompilationResult[] = []
  const emittedFiles: string[] = []

  const inputs = await glob(patterns)

  if( inputs.length === 0 ) {
    return {
      success: true,
      emittedFiles: [],
    } as const
  }

  for( const input of inputs ) {
    const source = await fs.promises.readFile(input, {
      encoding: 'utf-8',
    })

    const resultEither = compileSource({
      filename: input,
      source,
      module: options.module,
    })

    if( isLeft(resultEither) ) {
      return {
        success: false,
        diagnostics: unwrap(resultEither),
      } as const
    }

    const result = unwrap(resultEither)
    compilationResults.push(result)
  }

  const flatResults = compilationResults.flat()
  const declarations = getDeclarations(flatResults)

  await generateScaffolding(options)
  await writeBaseFiles(declarations, options)

  for( const decl of declarations ) {
    const declPath = getDeclarationPath(decl, options.outDir)
    const jsPath = addJsExtension(declPath, options)
    const tsPath = addDtsExtension(declPath, options)

    await fs.promises.writeFile(jsPath, decl.js)
    await fs.promises.writeFile(tsPath, decl.ts)

    emittedFiles.push(...[
      jsPath,
      tsPath,
    ])
  }

  return {
    success: true,
    emittedFiles,
  } as const
}

