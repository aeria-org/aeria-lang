import type { Declaration, RootPackageJson, BuildOptions } from './types.js'
import * as compiler from 'aeria-compiler'

const DECLARATION_PATH: Record<compiler.DeclarationType, string> = {
  collection: 'collections'
}

export const addJsExtension = (filename: string, options: BuildOptions) => {
  switch( options.module ) {
    case 'esnext': return `${filename}.mjs`
    case 'commonjs': return `${filename}.js`
  }
}

export const getDeclarationPath = (decl: Declaration, outDir: string) => {
  const doubleSlashesRemoved = outDir.replace('//', '/')
  const sanitizedOutDir = doubleSlashesRemoved.endsWith('/')
    ? doubleSlashesRemoved
    : `${doubleSlashesRemoved}/`

  return `${sanitizedOutDir}${DECLARATION_PATH[decl.type]}/${decl.name}`
}

export const generateRootPackageJson = (options: BuildOptions) => {
  const json: RootPackageJson = {
    main: addJsExtension('index', options),
    types: 'index.d.ts',
    exports: {
      './collections': {
        require: './collections/index.js',
        import: './collections/index.mjs',
        types: './collections/index.d.ts',
      }
    },
  }

  return json
}

export const generateCollectionsIndexJs = (declarations: Declaration[], options: BuildOptions) => {
  let source = ''

  for( const decl of declarations ) {
    if( decl.type !== 'collection' ) {
      continue
    }

    const importPath = addJsExtension(`./${decl.name}`, options)
    switch( options.module ) {
      case 'esnext': source += `export * from '${importPath}'\n`; break
      case 'commonjs': source += `exports['${decl.name}'] = require('${importPath}')['${decl.name}']\n`; break
    }
  }

  return source
}

export const generateCollectionsIndexDts = (declarations: Declaration[], options: BuildOptions) => {
  let source = ''

  for( const decl of declarations ) {
    if( decl.type !== 'collection' ) {
      continue
    }
    const importPath = addJsExtension(`./${decl.name}`, options)
    source += `export * from '${importPath}'\n`
  }

  return source
}

