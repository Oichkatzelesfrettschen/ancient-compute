#!/usr/bin/env python3
"""
Update repo baseline JSON for a given arch using current summary.json.

Usage:
  python3 scripts/minix_set_baseline.py --arch i386 \
    --summary metrics/minix/i386/summary.json \
    --out INFRASTRUCTURE_AND_DEPLOYMENT/MINIX_BASELINES/i386.json
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument('--arch', required=True)
    ap.add_argument('--summary', required=True)
    ap.add_argument('--out', required=True)
    args = ap.parse_args()

    summary = json.loads(Path(args.summary).read_text())
    avg = summary.get('avg_ms')
    if not isinstance(avg, (int, float)):
        raise SystemExit('avg_ms missing in summary.json')

    baseline = {'avg_ms': int(avg), 'note': f'Baseline updated from {args.summary}'}
    Path(args.out).write_text(json.dumps(baseline, indent=2))
    print(f'Baseline for {args.arch} updated at {args.out} => avg_ms={int(avg)}')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

