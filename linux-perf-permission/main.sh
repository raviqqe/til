#!/bin/sh

set -ex

sudo sh -c 'echo 0 >/proc/sys/kernel/kptr_restrict'
sudo sh -c 'echo 0 >/proc/sys/kernel/perf_event_paranoid'
