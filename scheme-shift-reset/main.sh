#!/bin/sh

set -e

if [ -n "$CI" ]; then
  brew install minimal-racket
fi

racket --script ./main.scm
