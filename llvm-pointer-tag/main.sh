#!/bin/sh

set -ex

llc -O3 main.ll
