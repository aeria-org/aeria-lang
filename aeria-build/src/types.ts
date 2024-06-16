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
  name: compiler.CollectionName
  type: compiler.DeclarationType
  js: compiler.JsCode
  ts: compiler.TsCode
}

export type BuildOptions = {
  module: compiler.TargetModule
  outDir: string
  dryRun?: boolean
}

export type NormalizedSpan = {
  start: {
    index: number
    line: number
    character: number
  }
  end: {
    index: number
    line: number
    character: number
  }
}

