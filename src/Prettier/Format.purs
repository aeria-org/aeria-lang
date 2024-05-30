module Prettier.Format (formatTypescript, formatJavascript) where

import Prelude

import Control.Promise (Promise, toAff)
import Effect.Aff (Aff)

foreign import formatTypescriptImpl
  :: String
  -> Promise String

formatTypescript
  :: String
  -> Aff String
formatTypescript = formatTypescriptImpl >>> toAff

foreign import formatJavascriptImpl
  :: String
  -> Promise String

formatJavascript
  :: String
  -> Aff String
formatJavascript = formatJavascriptImpl >>> toAff

