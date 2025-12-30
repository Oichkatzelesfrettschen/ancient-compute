#!/usr/bin/env bash
set -euo pipefail
stamp=$(date -Iseconds | tr ':' '_')
mkdir -p output/snapshots/$stamp
cp -f output/*metrics*.csv output/snapshots/$stamp/ || true
cp -f output/env_fingerprint.csv output/snapshots/$stamp/ || true
echo "snapshot created: $stamp"
