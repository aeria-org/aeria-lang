import type { CompilationTarget, Declaration, NormalizedSpan } from './types.js'
import * as compiler from '@aeria-lang/compiler'

export const compileSource = (target: CompilationTarget) => {
  return compiler.compile(target.filename)(target.source)(target.module)
}

export const getDeclarations = (result: compiler.CompilationResult) => {
  const declarations: Map<string, Declaration> = new Map()

  for( const [decltype, name, sourcejs, sourcets] of result ) {
    const key = `${decltype}.${name}`
    declarations.set(key, {
      name,
      type: decltype,
      js: sourcejs,
      ts: sourcets,
    })
  }

  return Array.from(declarations.values())
}

export const isLeft = <L>(obj: compiler.Either<L, unknown>): obj is compiler.Left<L> => {
  return obj instanceof compiler.Left
}

export const unwrap = <T>(obj: { value0: T }) => {
  return obj.value0
}

export const getNormalizedSpan = (span: compiler.Span): NormalizedSpan => {
  return {
    start: {
      index: span.value0.value0,
      line: span.value0.value1 - 1,
      character: span.value0.value2,
    },
    end: {
      index: span.value0.value0,
      line: span.value0.value1 - 1,
      character: span.value0.value2,
    },
  }
}

