"use strict";

import prettier from "prettier";

const options = {
  semi: true,
  endOfLine: "lf",
}

export function formatTypescriptImpl(source) {
  return prettier.format(source, {
    parser: "babel-ts",
    ...options
  });
}

export function formatJavascriptImpl(source) {
  return prettier.format(source, {
    parser: "babel",
    ...options
  });
}
