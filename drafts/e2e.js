const { suite } = require('@aeriajs/tests')
const { personDescription } = require('./person')

suite([
  test('POST', '/person/insert', {
    token: {
      roles: [
        'root'
      ]
    },
    payload: {
      what: {
        name: 'joao'
      }
    },
    response: rightSchema({
      type: 'object',
      properties: {
        name: {
          literal: 'joao',
        },
        age: {
          literal: 50
        },
        job: {
          type: 'object',
          properties: {
            title: {
              literal: 'programmer'
            }
          }
        }
      }
    })
  }),
  test('POST', '/person/insert', {
    token: {
      roles: []
    },
    payload: {
      what: {
        name: 'joao'
      }
    },
    response: leftSchema({
      type: 'object',
      properties: {
        error: {
          literal: 'AUTHORIZATION_ERROR'
        }
      }
    })
  }),
  test('POST', '/person/get', {
    token: {
      roles: [
        'root'
      ]
    },
    payload: {
      filters: {
        name: 'joao'
      }
    },
    response: {
      type: 'object',
      required: [],
      properties: {
        job: {
          type: 'object',
          required: [],
          properties: {
            title: {
              literal: 'programmer'
            }
          }
        }
      }
    }
  }),
  test('POST', '/person/getAll', {
    token: {
      roles: [
        'root'
      ]
    },
    response: {
      type: 'array',
      items: {
        type: 'object',
        properties: personDescription.properties
      }
    }
  })
])

