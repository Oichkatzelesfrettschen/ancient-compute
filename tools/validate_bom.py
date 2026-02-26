#!/usr/bin/env python3
"""Validate Bill of Materials CSV per BOM_SCHEMA.md.

Checks:
- Required columns present
- No duplicate part_ids
- spec_ref is SOURCE:<id> or ASSUMPTION
- quantity is positive integer
- mass_kg is non-negative

Usage:
    PYTHONPATH=. python3 tools/validate_bom.py [path/to/bom.csv]
"""

from __future__ import annotations

import csv
import re
import sys
from pathlib import Path

DEFAULT_BOM = Path(__file__).resolve().parents[1] / "hardware_twin" / "bom" / "bom_v0.csv"

REQUIRED_COLUMNS = {"part_id", "subsystem", "component", "material", "quantity", "mass_kg", "spec_ref"}
SPEC_REF_RE = re.compile(r"^(SOURCE:[A-Z0-9_-]+|ASSUMPTION)$")


def validate_bom(path: Path) -> list[str]:
    """Validate a BOM CSV file. Returns list of error messages."""
    errors = []

    if not path.exists():
        return [f"File not found: {path}"]

    with open(path, "r", newline="") as f:
        reader = csv.DictReader(f)

        # Check required columns
        if reader.fieldnames is None:
            return ["Empty CSV file"]

        missing = REQUIRED_COLUMNS - set(reader.fieldnames)
        if missing:
            errors.append(f"Missing required columns: {sorted(missing)}")
            return errors

        seen_ids: set[str] = set()
        row_num = 1

        for row in reader:
            row_num += 1
            prefix = f"Row {row_num}"

            # part_id
            part_id = row.get("part_id", "").strip()
            if not part_id:
                errors.append(f"{prefix}: empty part_id")
            elif part_id in seen_ids:
                errors.append(f"{prefix}: duplicate part_id '{part_id}'")
            seen_ids.add(part_id)

            # subsystem
            if not row.get("subsystem", "").strip():
                errors.append(f"{prefix} ({part_id}): empty subsystem")

            # material
            if not row.get("material", "").strip():
                errors.append(f"{prefix} ({part_id}): empty material")

            # spec_ref
            spec_ref = row.get("spec_ref", "").strip()
            if not spec_ref:
                errors.append(f"{prefix} ({part_id}): empty spec_ref")
            elif not SPEC_REF_RE.match(spec_ref):
                errors.append(f"{prefix} ({part_id}): invalid spec_ref '{spec_ref}'")

            # quantity
            try:
                qty = int(row.get("quantity", "0"))
                if qty < 1:
                    errors.append(f"{prefix} ({part_id}): quantity must be >= 1, got {qty}")
            except ValueError:
                errors.append(f"{prefix} ({part_id}): quantity is not an integer")

            # mass_kg
            try:
                mass = float(row.get("mass_kg", "0"))
                if mass < 0:
                    errors.append(f"{prefix} ({part_id}): mass_kg must be >= 0, got {mass}")
            except ValueError:
                errors.append(f"{prefix} ({part_id}): mass_kg is not a number")

    return errors


def main() -> None:
    path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_BOM
    errors = validate_bom(path)

    if errors:
        print(f"FAIL: {len(errors)} error(s) in {path}")
        for e in errors:
            print(f"  - {e}")
        sys.exit(1)
    else:
        # Summary stats
        with open(path, "r", newline="") as f:
            reader = csv.DictReader(f)
            rows = list(reader)

        total_parts = sum(int(r["quantity"]) for r in rows)
        total_mass = sum(float(r["mass_kg"]) * int(r["quantity"]) for r in rows)
        sourced = sum(1 for r in rows if r["spec_ref"].startswith("SOURCE:"))
        assumed = sum(1 for r in rows if r["spec_ref"] == "ASSUMPTION")

        print(f"PASS: {path}")
        print(f"  Line items: {len(rows)}")
        print(f"  Total parts: {total_parts}")
        print(f"  Total mass: {total_mass:.1f} kg")
        print(f"  Sourced: {sourced}, Assumptions: {assumed}")


if __name__ == "__main__":
    main()
