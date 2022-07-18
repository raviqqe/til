#!/bin/sh

set -ex

for file in kptr_restrict perf_event_paranoid; do
  sudo sh -c "echo 0 >/proc/sys/kernel/$file"
done
