#!/usr/bin/env python3
"""Verify required Babbage simulation parameters and citations."""

from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Any

import yaml

REPO_ROOT = Path(__file__).resolve().parents[2]
SIM_SCHEMA = REPO_ROOT / "docs/simulation/sim_schema.yaml"
CITATIONS = REPO_ROOT / "docs/simulation/CITATIONS.md"

REQUIRED_PATHS = [
    ("tolerances", "gear_module_mm"),
    ("tolerances", "backlash_mm"),
    ("tolerances", "shaft_clearance_mm"),
    ("tolerances", "bearing_clearance_mm"),
    ("valve_gear_params", "lap_mm"),
    ("valve_gear_params", "lead_mm"),
    ("valve_gear_params", "cutoff_pct"),
    ("lubrication", "viscosity_cSt_40C"),
    ("lubrication", "schedule_hours"),
    ("lubrication", "schedule_cycles_min"),
    ("lubrication", "schedule_cycles_max"),
    ("mechanisms", "card_feed", "feed_rate_cards_per_min"),
    ("mechanisms", "card_feed", "jam_rate_per_1k"),
]

REQUIRED_CITATION_KEYS = [
    "gear_module_mm",
    "backlash_mm",
    "shaft_clearance_mm",
    "bearing_clearance_mm",
    "lap_mm",
    "lead_mm",
    "cutoff_pct",
    "viscosity_cSt_40C",
    "schedule_hours",
    "schedule_cycles_min",
    "schedule_cycles_max",
    "feed_rate_cards_per_min",
    "jam_rate_per_1k",
]


def _resolve_path(data: dict[str, Any], path: tuple[str, ...]) -> Any:
    current: Any = data
    for key in path:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def _is_placeholder(value: Any) -> bool:
    if value is None:
        return True
    if isinstance(value, str):
        upper = value.upper()
        return "TBD" in upper or "P. __" in upper
    return False


def _load_yaml(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        parsed = yaml.safe_load(handle)
    if not isinstance(parsed, dict):
        raise ValueError(f"{path} must contain a YAML mapping at root")
    return parsed


def _citation_key_lines(text: str) -> dict[str, str]:
    # Supports list lines in the form: "- key: description".
    mapping: dict[str, str] = {}
    for line in text.splitlines():
        match = re.match(r"^\s*-\s*([a-zA-Z0-9_]+)\s*:\s*(.+)$", line.strip())
        if match:
            mapping[match.group(1)] = match.group(2)
    return mapping


def main() -> int:
    failures: list[str] = []

    if not SIM_SCHEMA.exists():
        print(f"[ERROR] Missing schema: {SIM_SCHEMA}")
        return 2
    if not CITATIONS.exists():
        print(f"[ERROR] Missing citations: {CITATIONS}")
        return 2

    schema = _load_yaml(SIM_SCHEMA)
    citation_text = CITATIONS.read_text(encoding="utf-8")
    citation_lines = _citation_key_lines(citation_text)

    for path in REQUIRED_PATHS:
        value = _resolve_path(schema, path)
        if _is_placeholder(value):
            failures.append(f"Missing or placeholder schema value at {'.'.join(path)}")

    for key in REQUIRED_CITATION_KEYS:
        citation = citation_lines.get(key)
        if citation is None:
            failures.append(f"Missing citation entry for key '{key}'")
            continue
        upper = citation.upper()
        if "P. __" in upper or "TBD" in upper:
            failures.append(f"Placeholder citation for key '{key}'")

    if failures:
        print("Babbage simulation parameter verification FAILED")
        for failure in failures:
            print(f"- {failure}")
        return 1

    print("Babbage simulation parameter verification PASSED")
    print(f"- Checked required schema paths: {len(REQUIRED_PATHS)}")
    print(f"- Checked required citation keys: {len(REQUIRED_CITATION_KEYS)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
