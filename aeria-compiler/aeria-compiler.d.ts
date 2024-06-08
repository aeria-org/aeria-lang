export declare class Left<T> {
  value0: T
}

export declare class Right<T> {
  value0: T
}

export type Either<L, R> = Left<L> | Right<R>

export type TargetModule =
  | 'commonjs'
  | 'esnext'

export type DeclarationType =
  | 'collection'

export type CompilationOutput = readonly [
  string,
  string,
  string,
]

export type CompilationResult = CompilationOutput[]

export type CompilationError = {
  filepath: string
  source: string
  info: string
}

export const compile: (filename: string) => (source: string) => (targetModule: TargetModule) => Either<
  CompilationError,
  CompilationResult
>

