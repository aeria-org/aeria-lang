# Aeria Lang ![https://github.com/aeria-org/aeria-lang/actions/workflows/ci.yaml](https://github.com/aeria-org/aeria-lang/actions/workflows/ci.yaml/badge.svg)

```
collection PetToy {
  indexes {
    name
  }

  properties {
    name str
    brand enum @values(["dogs choice", "the pet company"])
  }
}

collection Pet {
  indexes {
    name
  }

  properties {
    name str
    toys {
      properties {
        favorite PetToy
      }
    }
  }
}
```
