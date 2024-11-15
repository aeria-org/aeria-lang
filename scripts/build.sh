#!/bin/sh

rm -rf packages/*/dist

pnpm --filter='./aeria-compiler' build \
  && pnpm --filter='./aeria-build' build \
  && pnpm --filter='./aeria-lang' build \
  && pnpm --filter='./playground' build 

