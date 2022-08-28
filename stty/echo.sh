#!/bin/sh

set -e

stty -echo

read foo

stty echo

echo Hello, $foo!
