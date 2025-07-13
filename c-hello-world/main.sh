#!/bin/sh

set -e

cc main.c
strip -o strip.out a.out
