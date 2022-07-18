#!/bin/sh

set -ex

sudo sh -c 'echo -1 >/proc/sys/kernel/perf_event_paranoid'
