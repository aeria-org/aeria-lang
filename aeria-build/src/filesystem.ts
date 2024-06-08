import type { Declaration } from './types.js'
import * as fs from 'fs'
import { getDeclarationPath } from './codegen.js'

type ScaffoldingOptions = {
  outDir: string
  dryRun?: boolean
}

export const generateScaffolding = async (declarations: Declaration[], options: ScaffoldingOptions = {
  outDir: './',
  dryRun: true
}) => {
  const result: string[] = []

  for( const decl of declarations ) {
    const declPath = getDeclarationPath(decl, options.outDir)
    result.push(declPath)
  }

  return result
}

