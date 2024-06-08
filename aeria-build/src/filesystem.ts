import type { CompilationResult } from 'aeria-compiler'
import type { Declaration, BuildOptions } from './types.js'
import * as fs from 'fs'
import * as path from 'path'
import { compile, isLeft, unwrapEither, getDeclarations } from './core.js'
import {
  addJsExtension,
  getDeclarationPath,
  generateRootPackageJson,
  generateCollectionsIndexJs,
  generateCollectionsIndexDts,

} from './codegen.js'

export const generateScaffolding = async (options: BuildOptions) => {
  const directories = [
    path.join(options.outDir, 'collections')
  ]

  if( !options.dryRun ) {
    for( const dir of directories ) {
      await fs.promises.mkdir(dir, {
        recursive: true
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
    JSON.stringify(generateRootPackageJson(options), null, 2)
  )
  await write(
    path.join(options.outDir, 'collections', addJsExtension('index', options)),
    generateCollectionsIndexJs(declarations, options)
  )
  await write(
    path.join(options.outDir, 'collections', 'index.d.ts'),
    generateCollectionsIndexDts(declarations, options)
  )

  return result
}

export const build = async (inputs: string[], options: BuildOptions) => {
  const compilationResults: CompilationResult[] = []
  const emittedFiles: string[] = []

  for( const input of inputs ) {
    const source = await fs.promises.readFile(input, {
      encoding: 'utf-8'
    })

    const resultEither = compile({
      filename: input,
      source,
      module: options.module,
    })

    if( isLeft(resultEither) ) {
      return <const>{
        success: false,
        diagnostics: unwrapEither(resultEither),
      }
    }

    const result = unwrapEither(resultEither)
    compilationResults.push(result)
  }

  const flatResults = compilationResults.flat()
  const declarations = getDeclarations(flatResults)

  await generateScaffolding(options)
  await writeBaseFiles(declarations, options)

  for( const decl of declarations ) {
    const declPath = getDeclarationPath(decl, options.outDir)
    const jsPath = addJsExtension(declPath, options)
    const tsPath = `${declPath}.d.ts`

    await fs.promises.writeFile(jsPath, decl.js)
    await fs.promises.writeFile(tsPath, decl.ts)

    emittedFiles.push(...[
      jsPath,
      tsPath,
    ])
  }

  return <const>{
    success: true,
    emittedFiles,
  }
}

