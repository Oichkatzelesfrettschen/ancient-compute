#!/usr/bin/env python3
"""
Compare summary average boot time against a baseline summary JSON.

Usage:
  python3 scripts/minix_compare_summary.py \
    --summary metrics/minix/i386/summary.json \
    --baseline INFRASTRUCTURE_AND_DEPLOYMENT/MINIX_BASELINES/i386.json \
    --tolerance 20

Exit non-zero on regression beyond tolerance.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path


def load_avg(path: Path) -> int:
    data = json.loads(path.read_text())
    val = data.get('avg_ms')
    if isinstance(val, (int, float)):
        return int(val)
    raise ValueError('avg_ms missing')


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument('--summary', required=True)
    ap.add_argument('--baseline', required=True)
    ap.add_argument('--tolerance', type=float, default=20.0)
    args = ap.parse_args()

    cur = load_avg(Path(args.summary))
    base = load_avg(Path(args.baseline))

    lower = base * (1 - args.tolerance / 100.0)
    upper = base * (1 + args.tolerance / 100.0)

    print(f'Baseline avg: {base} ms, Measured avg: {cur} ms, Tol: ±{args.tolerance}%')
    if cur > upper:
        print('REGRESSION: average exceeds tolerance')
        return 2
    print('OK')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

