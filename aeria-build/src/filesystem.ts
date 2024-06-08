import type { Declaration } from './types.js'
import * as fs from 'fs'
import * as path from 'path'
import {
  getDeclarationPath,
  generateRootPackageJson,
  generateRootIndexJs,
  generateRootIndexDts

} from './codegen.js'

export type FSOptions = {
  outDir: string
  dryRun?: boolean
}

export const generateScaffolding = async (declarations: Declaration[], options: FSOptions) => {
  const result: string[] = []

  for( const decl of declarations ) {
    const declPath = getDeclarationPath(decl, options.outDir)
    result.push(declPath)

    if( !options.dryRun ) {
      await fs.promises.mkdir(declPath, {
        recursive: true
      })
    }
  }

  return result
}

export const writeFiles = async (declarations: Declaration[], options: FSOptions) => {
  const result: Record<string, string> = {}

  const write = async (path: string, content: string) => {
    result[path] = content
    if( !options.dryRun ) {
      return fs.promises.writeFile(path, content)
    }
  }

  await write(
    path.join(options.outDir, 'package.json'),
    JSON.stringify(generateRootPackageJson(declarations), null, 2)
  )
  await write(
    path.join(options.outDir, 'index.js'),
    generateRootIndexJs(declarations)
  )
  await write(
    path.join(options.outDir, 'index.d.ts'),
    generateRootIndexDts(declarations)
  )

  return result
}

