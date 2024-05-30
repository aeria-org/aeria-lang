"use strict";

import prettier from "prettier";

export function formatTypescriptImpl(source) {
  return prettier.format(source, { parser: "typescript", semi: true });
}

export function formatJavascriptImpl(source) {
  return prettier.format(source, { parser: "babel", semi: true });
}
