#!/usr/bin/env python3
"""
Compare measured MINIX boot time against a baseline with tolerance.

Usage:
  python3 scripts/minix_regression_check.py \
    --metrics metrics/minix/i386/boot_time.json \
    --baseline-ms 65000 \
    --tolerance 20

Or:
  python3 scripts/minix_regression_check.py \
    --metrics metrics/minix/i386/boot_time.json \
    --baseline-json path/to/baseline.json \
    --tolerance 20

Exit code non-zero indicates regression beyond tolerance.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path


def load_boot_time_ms(path: Path) -> int:
    data = json.loads(path.read_text())
    val = data.get('boot_duration_ms')
    if isinstance(val, (int, float)):
        return int(val)
    raise ValueError('boot_duration_ms missing')


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument('--metrics', required=True, help='Path to measured boot_time.json')
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--baseline-ms', type=int, help='Baseline total boot time in ms')
    g.add_argument('--baseline-json', help='Path to baseline JSON containing boot_duration_ms')
    ap.add_argument('--tolerance', type=float, default=20.0, help='Allowed +/- percent from baseline (default 20)')
    args = ap.parse_args()

    measured = load_boot_time_ms(Path(args.metrics))
    if args.baseline_json:
        baseline = load_boot_time_ms(Path(args.baseline_json))
    else:
        baseline = args.baseline_ms

    lower = baseline * (1 - args.tolerance / 100.0)
    upper = baseline * (1 + args.tolerance / 100.0)

    print(f'Baseline: {baseline} ms, Measured: {measured} ms, Tolerance: ±{args.tolerance}%')
    if measured < lower:
        print('WARNING: Measured is faster than expected (below lower bound).')
        return 0
    if measured > upper:
        print('REGRESSION: Measured exceeds upper bound.')
        return 2
    print('OK: Within tolerance.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

