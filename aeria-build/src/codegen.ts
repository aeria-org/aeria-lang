import * as compiler from 'aeria-compiler'
import { Declaration, RootPackageJson } from './types.js'
import { getDeclarations } from './core.js'

const DECLARATION_PATH: Record<compiler.DeclarationType, string> = {
  collection: 'collections'
}

export const getDeclarationPath = (decl: Declaration, outDir = './') => {
  const doubleSlashesRemoved = outDir.replace('//', '/')
  const sanitizedOutDir = doubleSlashesRemoved.endsWith('/')
    ? doubleSlashesRemoved
    : `${doubleSlashesRemoved}/`

  return `${sanitizedOutDir}${DECLARATION_PATH[decl.type]}/${decl.name}`
}

export const generateRootPackageJson = (result: compiler.CompilationResult) => {
  const declarations = getDeclarations(result)
  const json: RootPackageJson = {
    exports: {}
  }

  for( const decl of declarations ) {
    const declPath = getDeclarationPath(decl)

    json.exports[declPath] = {
      require: `${declPath}.js`,
      import: `${declPath}.mjs`,
      types: `${declPath}.d.ts`,
    }
  }

  return json
}

export const generateRootIndexJs = (result: compiler.CompilationResult) => {
  const declarations = getDeclarations(result)
  let source = ''

  for( const decl of declarations ) {
    source += `export * from '${getDeclarationPath(decl)}'\n`
  }

  return source
}

export const generateRootIndexDts = (result: compiler.CompilationResult) => {
  const declarations = getDeclarations(result)
  let source = ''

  for( const decl of declarations ) {
    source += `export * from '${getDeclarationPath(decl)}'\n`
  }

  return source
}

