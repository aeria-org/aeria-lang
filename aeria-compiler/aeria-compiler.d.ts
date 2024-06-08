export declare class Left<T> {
  readonly value0: T
}

export declare class Right<T> {
  readonly value0: T
}

export declare class Diagnostic {
  readonly value0: CompilationError
}

export declare type Either<L, R> = Left<L> | Right<R>

export declare type TargetModule =
  | 'commonjs'
  | 'esnext'

export declare type DeclarationType =
  | 'collection'

export declare type CompilationOutput = readonly [
  string,
  string,
  string,
]

export declare type CompilationResult = CompilationOutput[]

export declare type CompilationError = {
  filepath: string
  source: string
  info: string
}

export declare const compile: (filename: string) => (source: string) => (targetModule: TargetModule) => Either<
  Diagnostic,
  CompilationResult
>

export declare const ppDiagnostic = (diagnostic: Diagnostic) => string

