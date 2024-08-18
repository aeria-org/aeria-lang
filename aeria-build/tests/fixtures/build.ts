import type { BuildOptions } from '../../src/index.js'
import { compileSource } from '../../src/index.js'

const DUMMY_FILENAME = 'dummy.aeria'

const validSource =
 `collection Pet {
  properties {
    name str
    breed enum @values(["dog", "cat"])
  }
}

collection Person {
  properties {
    name str
    age int
    pets []Pet
  }
}
`

const invalidSource = 
`collection Pet {
  properties {
    name str
    breed enum @values(["dog", "cat"])
  }
}

collection Person {
  properties {
    name str
    age int
    pets []PetXX
  }
}
`

export const buildOptions: BuildOptions = {
  outDir: 'node_modules/.test',
  module: 'esnext',
  dryRun: true,
}

export const validResultEither = compileSource({
  filename: DUMMY_FILENAME,
  source: validSource,
  module: 'esnext'
})

export const invalidResultEither = compileSource({
  filename: DUMMY_FILENAME,
  source: invalidSource,
  module: 'esnext'
})
