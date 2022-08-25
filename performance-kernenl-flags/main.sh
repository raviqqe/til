#!/bin/sh

set -e

echo 0 >/proc/sys/kernel/kptr_restrict
echo -1 >/proc/sys/kernel/perf_event_paranoid
