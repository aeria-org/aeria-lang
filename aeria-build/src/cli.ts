import type { TargetModule } from '@aeria-lang/compiler'
import { ppDiagnostic } from '@aeria-lang/compiler'
import { parseArgs } from 'util'
import { build } from './filesystem.js'

const { values: opts, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    module: {
      type: 'string',
      short: 'm',
    },
    outDir: {
      type: 'string',
      short: 'o',
    },
  },
})

const isValidModule = (module: string): module is TargetModule => {
  return [
    'esnext',
    'commojs',
  ].includes(module)
}

export const main = async () => {
  const targetModule = opts.module || 'esnext'
  if( !isValidModule(targetModule) ) {
    console.error(`invalid target module: "${targetModule}"`)
    process.exit(1)
  }

  if( !opts.outDir ) {
    console.error('missing -o flag')
    process.exit(1)
  }

  const result = await build(positionals, {
    outDir: opts.outDir,
    module: targetModule,
  })

  if( !result.success ) {
    console.error(ppDiagnostic(result.diagnostics))
    process.exit(1)
  }
}

