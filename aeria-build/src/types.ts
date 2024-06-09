import type * as compiler from '@aeria-lang/compiler'

export type CompilationTarget = {
  filename: string
  source: string
  module: compiler.TargetModule
}

export type RootPackageJson = {
  main: string
  types: string
  exports: Record<string, {
    require: string
    import: string
    types: string
  }>
}

export type Declaration = {
  name: string
  type: compiler.DeclarationType
  js: string
  ts: string
}

export type BuildOptions = {
  module: compiler.TargetModule
  outDir: string
  dryRun?: boolean
}

