import type { CompilationTarget, Declaration } from './types.js'
import * as compiler from '@aeria-lang/compiler'

export const compileSource = (target: CompilationTarget) => {
  return compiler.compile(target.filename)(target.source)(target.module)
}

export const getDeclarations = (result: compiler.CompilationResult): Declaration[] => {
  return result.map(([decltype, name, sourcejs, sourcets]) => ({
    name,
    type: decltype,
    js: sourcejs,
    ts: sourcets,
  }))
}

export const isLeft = <L>(obj: compiler.Either<L, unknown>): obj is compiler.Left<L> => {
  return obj instanceof compiler.Left
}

export const unwrapEither = <L, R>(obj: compiler.Either<L,R>) => {
  return obj.value0
}

