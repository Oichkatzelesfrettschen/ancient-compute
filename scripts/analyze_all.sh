#!/usr/bin/env bash
set -euo pipefail
python3 scripts/analyze_csv.py output/emulator_metrics_real.csv latency_s || true
python3 scripts/analyze_csv.py output/emulator_program_bench.csv latency_s || true
python3 scripts/analyze_csv.py output/backend_metrics.csv latency_s || true
python3 scripts/analyze_csv.py output/frontend_metrics.csv value || true
