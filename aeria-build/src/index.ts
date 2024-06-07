import * as compiler from 'aeria-compiler'

export type CompilationTarget = {
  filename: string
  source: string
  module: compiler.TargetModule
}

export const compile = (target: CompilationTarget) => {
  return compiler.compile(target.filename)(target.source)(target.module)
}

export const getCollectionNames = (result: compiler.CompilationResult[]) => {
  return result.map(([filename]) => filename)
}

export const isLeft = <L>(obj: compiler.Either<L, unknown>): obj is compiler.Left<L> => {
  return obj instanceof compiler.Left
}

export const unwrapEither = <L, R>(obj: compiler.Either<L,R>) => {
  return obj.value0
}

