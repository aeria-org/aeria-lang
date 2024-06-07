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

export type CompilationResult = readonly [
  string,
  string,
  string,
]

export type CompilationError = {
  filepath: string
  source: string
  info: string
}

export const compile: (filename: string) => (source: string) => (targetModule: TargetModule) => Either<CompilationError, CompilationResult[]>

