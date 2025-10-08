#!/bin/sh

set -e

for scheme in false true; do
  arguments="-- ${scheme:+--scheme} test/fixtures"

  node --run start $arguments
done
