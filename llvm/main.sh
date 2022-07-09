#!/bin/sh

set -ex

llc -S -O3 foo.ll
