const eslintConfigAeria = require('eslint-config-aeria')
eslintConfigAeria.languageOptions.parserOptions.project = ['./tsconfig.eslint.json']

module.exports = [eslintConfigAeria]
