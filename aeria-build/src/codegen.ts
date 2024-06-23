import type { Declaration, RootPackageJson, BuildOptions } from './types.js'
import type * as compiler from '@aeria-lang/compiler'

const DECLARATION_PATH: Record<compiler.DeclarationType, string> = {
  collection: 'collections',
  contract: 'contracts',
}

const removeTrailingCharacters = (source: string) => {
  return source.replace(/^(\s*|$)/mg, '')
}

const capitalize = (text: string) => {
  return text[0].toUpperCase() + text.slice(1)
}

export const addJsExtension = (filename: string, options: BuildOptions) => {
  switch( options.module ) {
    case 'esnext': return `${filename}.mjs`
    case 'commonjs': return `${filename}.js`
  }
}

export const addDtsExtension = (filename: string, options: BuildOptions) => {
  switch( options.module ) {
    case 'esnext': return `${filename}.d.mts`
    case 'commonjs': return `${filename}.d.ts`
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
    types: addDtsExtension('index', options),
    exports: {
      '.': {
        require: './index.js',
        import: './index.mjs',
        types: './index.d.ts',
      },
      './collections': {
        require: './collections/index.js',
        import: './collections/index.mjs',
        types: './collections/index.d.ts',
      },
    },
  }

  return json
}

export const generateRootIndexJs = (declarations: Declaration[], options: BuildOptions) => {
  switch( options.module ) {
    case 'esnext': return removeTrailingCharacters(`
      export * as collections from '${addJsExtension('./collections/index', options)}'
      ${declarations.map((decl) => {
    return `
          export { ${`extend${capitalize(decl.name)}Collection`} } from '${addJsExtension(`./collections/${decl.name}`, options)}'
        `
  }).join('\n')}
    `)
    case 'commonjs': return removeTrailingCharacters(`
      exports.collections = require('${addJsExtension('./collections/index', options)}')
      ${declarations.map((decl) => {
    return `
          exports.${`extend${capitalize(decl.name)}Collection`} = require('${addJsExtension(`./collections/${decl.name}`, options)}')
        `
  })}
    `)
  }
}

export const generateRootIndexDts = (declarations: Declaration[], options: BuildOptions) => {
  return removeTrailingCharacters(`
    export * as collections from '${addJsExtension('./collections/index', options)}'
    ${declarations.map((decl) => {
    return `
        export { ${`extend${capitalize(decl.name)}Collection`} } from '${addJsExtension(`./collections/${decl.name}`, options)}'
      `
  }).join('\n')}
  `)
}

export const generateCollectionsIndexJs = (declarations: Declaration[], options: BuildOptions) => {
  if( declarations.length === 0 ) {
    switch( options.module ) {
      case 'esnext': return 'export {}'
      case 'commonjs': return 'exports = {}'
    }
  }

  let source = ''
  for( const decl of declarations ) {
    if( decl.type !== 'collection' ) {
      continue
    }

    const importPath = addJsExtension(`./${decl.name}`, options)
    switch( options.module ) {
      case 'esnext':
        source += `export { ${decl.name} } from '${importPath}'\n`
        break
      case 'commonjs':
        source += `exports['${decl.name}'] = require('${importPath}')['${decl.name}']\n`
        break
    }
  }

  return source
}

export const generateCollectionsIndexDts = (declarations: Declaration[], options: BuildOptions) => {
  if( declarations.length === 0 ) {
    return 'export {}'
  }

  let source = ''
  for( const decl of declarations ) {
    if( decl.type !== 'collection' ) {
      continue
    }
    const importPath = addJsExtension(`./${decl.name}`, options)
    source += `export { ${decl.name} } from '${importPath}'\n`
  }

  return source
}

