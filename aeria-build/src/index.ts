import * as compiler from 'aeria-compiler'

export type CompilationTarget = {
  filename: string
  source: string
  module: compiler.TargetModule
}

export type RootPackageJson = {
  exports: Record<string, {
    require: string
    import: string
    types: string
  }>
}

export const compile = (target: CompilationTarget) => {
  return compiler.compile(target.filename)(target.source)(target.module)
}

export const getDeclarations = (result: compiler.CompilationResult) => {
  return result.map(([filename]) => filename)
}

export const isLeft = <L>(obj: compiler.Either<L, unknown>): obj is compiler.Left<L> => {
  return obj instanceof compiler.Left
}

export const unwrapEither = <L, R>(obj: compiler.Either<L,R>) => {
  return obj.value0
}

export const generateRootPackageJson = (result: compiler.CompilationResult) => {
  const declNames = getDeclarations(result)
  const json: RootPackageJson = {
    exports: {}
  }

  for( const declName of declNames ) {
    const requirePath = `./${declName}.js`
    const importPath = `./${declName}.mjs`
    const typesPath = `./${declName}.d.ts`
    json.exports[declName] = {
      require: requirePath,
      import: importPath,
      types: typesPath,
    }
  }

  return json
}

export const generateRootIndexJs = (result: compiler.CompilationResult) => {
  const declNames = getDeclarations(result)
  let source = ''

  for( const declName of declNames ) {
    source += `export * from '${declName}'\n`
  }

  return source
}

export const generateRootIndexDts = (result: compiler.CompilationResult) => {
  const declNames = getDeclarations(result)
  let source = ''

  for( const declName of declNames ) {
    source += `export * from '${declName}'\n`
  }

  return source
}

