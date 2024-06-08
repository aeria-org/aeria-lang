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

export type Declaration = {
  name: string
  type: compiler.DeclarationType
}

