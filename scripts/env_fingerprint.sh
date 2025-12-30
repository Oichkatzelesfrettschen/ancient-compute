#!/usr/bin/env bash
set -euo pipefail
printf "key,value\n"
printf "datetime,%s\n" "$(date -Iseconds)"
printf "hostname,%s\n" "$(hostname)"
printf "os,%s\n" "$(uname -a)"
printf "cpu,%s\n" "$(grep -m1 'model name' /proc/cpuinfo | cut -d: -f2 | xargs || echo unknown)"
printf "ram_mb,%s\n" "$(awk '/MemTotal/{print int($2/1024)}' /proc/meminfo)"
printf "python,%s\n" "$(python3 --version 2>&1)"
