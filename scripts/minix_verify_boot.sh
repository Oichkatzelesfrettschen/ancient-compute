#!/usr/bin/env bash
set -euo pipefail

# Verify that a boot log contains a login prompt.
# Usage: ./scripts/minix_verify_boot.sh metrics/minix/i386/boot.log

LOG="${1:-}"
if [[ -z "$LOG" || ! -f "$LOG" ]]; then
  echo "Usage: $0 /path/to/boot.log" >&2
  exit 2
fi

if LC_ALL=C grep -E "(^|\s)login:|MINIX.*boot|Scheduler.*ready" "$LOG" >/dev/null; then
  echo "OK: Boot markers found in $LOG"
  exit 0
else
  echo "ERROR: No boot markers/login prompt found in $LOG" >&2
  exit 1
fi

