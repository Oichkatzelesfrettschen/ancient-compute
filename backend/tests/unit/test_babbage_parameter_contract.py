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


class TestSimSchemaStructure:
    def _schema(self) -> dict[str, Any]:
        return yaml.safe_load(SIM_SCHEMA.read_text(encoding="utf-8"))

    def test_schema_loads_as_dict(self) -> None:
        schema = self._schema()
        assert isinstance(schema, dict)

    def test_required_top_level_sections_present(self) -> None:
        schema = self._schema()
        required_sections = [
            "materials",
            "tolerances",
            "lubrication",
            "valve_gear_params",
            "mechanisms",
        ]
        for section in required_sections:
            assert section in schema, f"missing section: {section}"

    def test_materials_section_is_list(self) -> None:
        schema = self._schema()
        assert isinstance(schema["materials"], list)

    def test_materials_count_at_least_14(self) -> None:
        schema = self._schema()
        assert len(schema["materials"]) >= 14

    def test_each_material_has_name_field(self) -> None:
        schema = self._schema()
        for mat in schema["materials"]:
            assert "name" in mat, f"material missing name: {mat}"

    def test_each_material_has_density(self) -> None:
        schema = self._schema()
        for mat in schema["materials"]:
            assert "density_kg_m3" in mat, f"material {mat.get('name')} missing density"

    def test_brass_material_present(self) -> None:
        schema = self._schema()
        names = [m["name"] for m in schema["materials"]]
        assert "brass" in names

    def test_new_materials_present(self) -> None:
        schema = self._schema()
        names = [m["name"] for m in schema["materials"]]
        for mat in ["corinthian_bronze", "ivory", "boxwood", "aluminum_alloy_1940s"]:
            assert mat in names, f"expected material {mat} not found"

    def test_registers_digits_is_40(self) -> None:
        schema = self._schema()
        assert schema.get("registers", {}).get("digits") == 40

    def test_registers_base_is_10(self) -> None:
        schema = self._schema()
        assert schema.get("registers", {}).get("base") == 10

    def test_opcodes_section_present(self) -> None:
        schema = self._schema()
        assert "opcodes" in schema

    def test_tolerances_all_values_are_positive(self) -> None:
        schema = self._schema()
        tol = schema.get("tolerances", {})
        for key, val in tol.items():
            if isinstance(val, (int, float)):
                assert val >= 0, f"tolerance {key} should be >= 0, got {val}"

    def test_lubrication_schedule_cycles_max_ge_min(self) -> None:
        schema = self._schema()
        lub = schema.get("lubrication", {})
        assert lub.get("schedule_cycles_max", 0) >= lub.get("schedule_cycles_min", 0)


class TestCitationsFile:
    def _text(self) -> str:
        return CITATIONS.read_text(encoding="utf-8")

    def test_citations_file_exists(self) -> None:
        assert CITATIONS.exists()

    def test_citations_is_non_empty(self) -> None:
        text = self._text()
        assert len(text) > 100

    def test_no_placeholder_page_numbers(self) -> None:
        assert "p. __" not in self._text()

    def test_no_tbd_in_citations(self) -> None:
        assert "TBD" not in self._text().upper()
