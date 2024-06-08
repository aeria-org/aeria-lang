import * as compiler from 'aeria-compiler'
import { CompilationTarget, Declaration } from './types.js'

export const compile = (target: CompilationTarget) => {
  return compiler.compile(target.filename)(target.source)(target.module)
}

export const getDeclarations = (result: compiler.CompilationResult): Declaration[] => {
  return result.map(([name]) => ({
    name,
    type: 'collection'
  }))
}

export const isLeft = <L>(obj: compiler.Either<L, unknown>): obj is compiler.Left<L> => {
  return obj instanceof compiler.Left
}

export const unwrapEither = <L, R>(obj: compiler.Either<L,R>) => {
  return obj.value0
}

