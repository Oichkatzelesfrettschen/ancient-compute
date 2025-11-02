#!/usr/bin/env python3
"""
Aggregate MINIX run statistics across metrics/minix/<arch>/runs.

Outputs a JSON summary with count, min, max, average of boot_duration_ms,
and a list of recent runs with timestamps and durations.

Usage:
  python3 scripts/minix_aggregate.py --root metrics/minix --arch i386 --out metrics/minix/i386/summary.json
"""
from __future__ import annotations

import json
from pathlib import Path
from statistics import mean
from typing import List, Dict, Any
import argparse


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument('--root', required=True)
    ap.add_argument('--arch', required=True)
    ap.add_argument('--out', required=True)
    args = ap.parse_args()

    runs_dir = Path(args.root).resolve() / args.arch / 'runs'
    runs: List[Dict[str, Any]] = []
    for run in sorted(runs_dir.glob('*')):
        bt = run / 'boot_time.json'
        if bt.exists():
            try:
                data = json.loads(bt.read_text())
                rid = run.name
                dur = data.get('boot_duration_ms')
                ts = data.get('timestamp')
                runs.append({'run_id': rid, 'timestamp': ts, 'boot_duration_ms': dur})
            except Exception:
                pass

    durations = [r['boot_duration_ms'] for r in runs if isinstance(r.get('boot_duration_ms'), (int, float))]
    summary: Dict[str, Any] = {
        'arch': args.arch,
        'run_count': len(runs),
        'min_ms': min(durations) if durations else None,
        'max_ms': max(durations) if durations else None,
        'avg_ms': int(mean(durations)) if durations else None,
        'recent': sorted(runs, key=lambda x: (x.get('timestamp') or '', x['run_id']))[-10:],
    }

    out_path = Path(args.out)
    out_path.write_text(json.dumps(summary, indent=2))
    print(f'Wrote summary to {out_path}')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

