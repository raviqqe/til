#!/bin/sh

set -e

echo -n 'Name: '

stty -echo

read foo

stty echo

echo
echo Hello, $foo!
