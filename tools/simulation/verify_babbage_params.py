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
    ("mechanisms", "main_shaft", "diameter_mm"),
    ("mechanisms", "main_shaft", "length_mm"),
    ("mechanisms", "bearings", "bore_diameter_mm"),
    ("mechanisms", "bearings", "pv_limit_MPa_m_s"),
    ("structural", "machine_mass_kg"),
]

# Material names that must be present in the materials list
REQUIRED_MATERIALS = ["brass", "steel", "cast_iron", "phosphor_bronze", "spring_steel"]

# Fields required on every material entry
REQUIRED_MATERIAL_FIELDS = [
    "density_kg_m3",
    "friction_coeff",
    "youngs_modulus_GPa",
    "poissons_ratio",
    "yield_strength_MPa",
    "ultimate_tensile_strength_MPa",
    "endurance_limit_MPa",
    "thermal_expansion_coeff_per_K",
    "specific_heat_J_kgK",
    "thermal_conductivity_W_mK",
    "hardness_HB",
    "electrical_resistivity_ohm_m",
    "magnetic_permeability_relative",
    "creep_threshold_C",
    "temperature_range_C",
    "source",
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
    "brass_properties",
    "steel_properties",
    "cast_iron_properties",
    "phosphor_bronze_properties",
    "spring_steel_properties",
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

    # Validate materials list
    materials_list = schema.get("materials", [])
    if not isinstance(materials_list, list):
        failures.append("'materials' must be a YAML list")
    else:
        mat_by_name = {m["name"]: m for m in materials_list if isinstance(m, dict) and "name" in m}
        for mat_name in REQUIRED_MATERIALS:
            if mat_name not in mat_by_name:
                failures.append(f"Missing required material: {mat_name}")
            else:
                entry = mat_by_name[mat_name]
                for field in REQUIRED_MATERIAL_FIELDS:
                    if field not in entry or _is_placeholder(entry[field]):
                        failures.append(f"Material '{mat_name}' missing or placeholder field: {field}")

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

    material_count = len(materials_list) if isinstance(materials_list, list) else 0
    field_checks = material_count * len(REQUIRED_MATERIAL_FIELDS)
    print("Babbage simulation parameter verification PASSED")
    print(f"- Checked required schema paths: {len(REQUIRED_PATHS)}")
    print(f"- Checked required materials: {len(REQUIRED_MATERIALS)} ({field_checks} fields)")
    print(f"- Checked required citation keys: {len(REQUIRED_CITATION_KEYS)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
