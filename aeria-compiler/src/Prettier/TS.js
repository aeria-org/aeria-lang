"use strict";

import ts from "typescript";

export function formatTS(code) {
  const sourceFile = ts.createSourceFile('<stdin>', code, ts.ScriptTarget.Latest, true);

  const printer = ts.createPrinter({
    newLine: ts.NewLineKind.LineFeed,
    removeComments: false,
  });

  return printer.printFile(sourceFile);
}
