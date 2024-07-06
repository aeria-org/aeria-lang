"use strict";

import esprima from "esprima";
import escodegen from "escodegen";

export function formatJS(code) {
  const ast = esprima.parseScript(code, { tolerant: true });
  return escodegen.generate(ast);
}
