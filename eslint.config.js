const eslintConfigAeria = require('eslint-config-aeria')
eslintConfigAeria.languageOptions.parserOptions.project = ['./tsconfig.eslint.json']
eslintConfigAeria.languageOptions.parserOptions.tsconfigRootDir = __dirname


module.exports = [eslintConfigAeria]
