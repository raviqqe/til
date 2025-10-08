#!/bin/sh

set -e

for scheme in false true; do
  arguments="-- $($scheme && echo '--scheme' || :) test/fixtures"

  for script in start start:tex; do
    node --run $script $arguments
  done
done
