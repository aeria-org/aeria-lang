module Main where

import Prelude

import Aeria.Codegen.Javascript.Tree (Output(..))
import Aeria.Driver (compile)
import Effect (Effect)

example :: String
example = """
  collection Profile {
    properties {
      first_name            str
      last_name             str
      profile_picture File  @accept(["image/*"])
    }

    getters {
      name    @js (values) => {
        return `${values.first_name} ${values.last_name}`;
      }
      @end
    }
  }

  collection User {
    properties {
      username      str
      password      str
      profile       Profile   @indexes([first_name, last_name])
    }
  }
"""

main :: Effect Unit
main = compile example "./sonata" CJs
