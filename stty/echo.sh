#!/bin/sh

set -e

echo Name?:

stty -echo

read foo

stty echo

echo Hello, $foo!
