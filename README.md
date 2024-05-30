# Aeria Lang ![https://github.com/aeria-org/aeria-lang/actions/workflows/ci.yaml](https://github.com/aeria-org/aeria-lang/actions/workflows/ci.yaml/badge.svg)

```
collection PetToy {
  properties {
    name str
    brand enum @values(["dogs choice", "the pet company"])
  }
}

collection Pet {
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
