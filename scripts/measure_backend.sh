#!/usr/bin/env bash
set -euo pipefail
runs=10
url=${BACKEND_URL:-http://localhost:8000/docs}
printf "run,latency_s,ok\n"
for i in $(seq 1 $runs); do
  t0=$(date +%s%N)
  if curl -s -o /dev/null --max-time 5 "$url"; then
    ok=1
  else
    ok=0
  fi
  t1=$(date +%s%N)
  dt=$(echo "scale=6; ($t1-$t0)/1000000000" | bc)
  printf "%s,%s,%s\n" "$i" "$dt" "$ok"
done
