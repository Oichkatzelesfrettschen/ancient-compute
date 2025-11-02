#!/usr/bin/env python3
"""
Merge MINIX boot metrics and logs into canonical artifacts.

Inputs:
  --measurements <dir>   # directory containing /<arch>/measurement files
  --arch i386|arm
  --out <dir>            # destination (e.g., metrics/minix/i386)

Produces:
  boot_time.json         # merged JSON: total + markers + meta
  resource_timeseries.csv (copied if present)
  boot.log, qemu-debug.log (copied if present)
"""
from __future__ import annotations

import argparse
import json
import shutil
from pathlib import Path
from typing import Optional, Dict, Any


def newest(pattern: str) -> Optional[Path]:
    files = sorted(Path().glob(pattern), key=lambda p: p.stat().st_mtime)
    return files[-1] if files else None


def load_json(path: Optional[Path]) -> Dict[str, Any]:
    if not path or not path.exists():
        return {}
    try:
        return json.loads(path.read_text())
    except Exception:
        return {}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--measurements", required=True)
    ap.add_argument("--arch", choices=["i386", "arm"], default="i386")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    mroot = Path(args.measurements).resolve() / args.arch
    out = Path(args.out).resolve()
    out.mkdir(parents=True, exist_ok=True)

    # Find latest measurement and report
    meas = newest(str(mroot / "measurements-*.json"))
    report = newest(str(mroot / "boot-report-*.json"))

    meas_j = load_json(meas)
    report_j = load_json(report)

    # Merge
    merged: Dict[str, Any] = {
        "arch": args.arch,
        "timestamp": report_j.get("timestamp") or meas_j.get("timestamp"),
        "qemu_mode": meas_j.get("qemu_mode"),
        "boot_duration_ms": meas_j.get("boot_duration_ms"),
        "markers": report_j.get("boot_markers", {}),
        "paths": {
            "boot_log": meas_j.get("boot_log"),
            "debug_log": meas_j.get("debug_log"),
            "disk_image": meas_j.get("disk_image"),
        },
    }

    # Write canonical JSON
    (out / "boot_time.json").write_text(json.dumps(merged, indent=2))

    # Copy resource CSV and logs if present
    res_csv = mroot / "resource_timeseries.csv"
    if res_csv.exists():
        shutil.copy2(res_csv, out / "resource_timeseries.csv")

    # Copy last boot and debug logs if present
    boot_log = newest(str(mroot / "boot-*.log"))
    if boot_log and boot_log.exists():
        shutil.copy2(boot_log, out / "boot.log")

    debug_log = newest(str(mroot / "qemu-debug-*.log"))
    if debug_log and debug_log.exists():
        shutil.copy2(debug_log, out / "qemu-debug.log")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

