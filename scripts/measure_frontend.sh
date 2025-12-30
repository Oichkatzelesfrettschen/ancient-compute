#!/usr/bin/env bash
set -euo pipefail
runs=5
printf "metric,value\n"
for i in $(seq 1 $runs); do
  t0=$(date +%s%N)
  (cd frontend && pnpm -s build) >/dev/null 2>&1 || true
  t1=$(date +%s%N)
  dt=$(echo "scale=3; ($t1-$t0)/1000000000" | bc)
  printf "build_time_s,%s\n" "$dt"
  t2=$(date +%s%N)
  (cd frontend && pnpm -s test) >/dev/null 2>&1 || true
  t3=$(date +%s%N)
  dt2=$(echo "scale=3; ($t3-$t2)/1000000000" | bc)
  printf "test_time_s,%s\n" "$dt2"
done
