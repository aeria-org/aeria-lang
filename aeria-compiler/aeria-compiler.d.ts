export declare class Left<T> {
  readonly value0: T
}

export declare class Right<T> {
  readonly value0: T
}

export declare class CompilationError {
  readonly value0: Diagnostic
}

export declare class SourcePos {
  readonly value0: number
  readonly value1: number
  readonly value2: number
}

export declare class Span {
  readonly value0: SourcePos
  readonly value1: SourcePos
}

export declare type Either<L, R> = Left<L> | Right<R>

export declare type TargetModule =
  | 'commonjs'
  | 'esnext'

export declare type DeclarationType =
  | 'collection'
  | 'contract'

export declare type CollectionName = string

export declare type JsCode = string

export declare type TsCode = string

export declare type CompilationOutput = readonly [
  DeclarationType,
  CollectionName,
  JsCode,
  TsCode,
]

export declare type CompilationResult = CompilationOutput[]

export declare type Diagnostic = {
  filepath: string
  source: string
  info: string
  span: Span
}

export declare const compile: (filename: string) => (source: string) => (targetModule: TargetModule) => Either<
  CompilationError,
  CompilationResult
>

export declare const ppDiagnostic: (error: CompilationError) => string

