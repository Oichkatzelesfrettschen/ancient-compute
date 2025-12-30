#!/usr/bin/env bash
set -euo pipefail
out=docs/sources/cache
mkdir -p "$out"
awk '/^  url:/ {print $2}' docs/sources/sources.yaml | tr -d '"' | while read -r url; do
  name=$(basename "$url" | sed 's/[^A-Za-z0-9._-]/_/g')
  [ -z "$name" ] && name="download_$(date +%s).bin"
  echo "Fetching: $url -> $out/$name"
  curl -L --fail --retry 3 --connect-timeout 10 -o "$out/$name" "$url" || echo "WARN: fetch failed (offline?) $url"
done
