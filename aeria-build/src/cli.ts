import { build } from './filesystem.js'

export const main = async () => {
  const result = await build(['/tmp/1.aeria'], {
    outDir: '/tmp/banana1',
    module: 'commonjs',
  })

  console.log(result)
  if( !result.success ) {
    process.exit(1)
  }
}


