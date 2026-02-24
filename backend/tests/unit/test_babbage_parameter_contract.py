"""Unit contract tests for Babbage simulation parameter integrity."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml


REPO_ROOT = Path(__file__).resolve().parents[3]
SIM_SCHEMA = REPO_ROOT / "docs/simulation/sim_schema.yaml"
CITATIONS = REPO_ROOT / "docs/simulation/CITATIONS.md"


def _resolve_path(data: dict[str, Any], path: tuple[str, ...]) -> Any:
    current: Any = data
    for key in path:
        if not isinstance(current, dict) or key not in current:
            return None
        current = current[key]
    return current


def test_required_babbage_fields_are_present_and_not_tbd() -> None:
    schema = yaml.safe_load(SIM_SCHEMA.read_text(encoding="utf-8"))
    assert isinstance(schema, dict)

    required = [
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

    for path in required:
        value = _resolve_path(schema, path)
        assert value is not None, f"missing required path: {'.'.join(path)}"
        if isinstance(value, str):
            assert "TBD" not in value.upper(), f"placeholder at path: {'.'.join(path)}"


def test_required_babbage_numeric_ranges_are_sane() -> None:
    schema = yaml.safe_load(SIM_SCHEMA.read_text(encoding="utf-8"))
    assert isinstance(schema, dict)

    assert 0 < _resolve_path(schema, ("tolerances", "gear_module_mm")) < 10
    assert 0 <= _resolve_path(schema, ("tolerances", "backlash_mm")) < 1
    assert 0 <= _resolve_path(schema, ("tolerances", "shaft_clearance_mm")) < 1
    assert 0 <= _resolve_path(schema, ("tolerances", "bearing_clearance_mm")) < 1
    assert 0 <= _resolve_path(schema, ("valve_gear_params", "cutoff_pct")) <= 100
    assert _resolve_path(schema, ("lubrication", "viscosity_cSt_40C")) > 0
    assert _resolve_path(schema, ("lubrication", "schedule_hours")) > 0
    assert _resolve_path(schema, ("lubrication", "schedule_cycles_min")) > 0
    assert _resolve_path(schema, ("lubrication", "schedule_cycles_max")) >= _resolve_path(
        schema, ("lubrication", "schedule_cycles_min")
    )
    assert _resolve_path(schema, ("mechanisms", "card_feed", "feed_rate_cards_per_min")) > 0
    assert _resolve_path(schema, ("mechanisms", "card_feed", "jam_rate_per_1k")) >= 0


def test_citations_cover_required_babbage_fields() -> None:
    text = CITATIONS.read_text(encoding="utf-8")

    required_keys = [
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

    for key in required_keys:
        assert f"- {key}:" in text
    assert "p. __" not in text
