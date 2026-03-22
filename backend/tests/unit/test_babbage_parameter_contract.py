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


class TestSimSchemaMaterialProperties:
    """Each material entry must have the minimum required property fields."""

    def _schema(self) -> dict:
        return yaml.safe_load(SIM_SCHEMA.read_text(encoding="utf-8"))

    def _materials(self) -> list:
        return self._schema()["materials"]

    def test_all_materials_have_name(self) -> None:
        for mat in self._materials():
            assert "name" in mat

    def test_all_materials_have_density(self) -> None:
        for mat in self._materials():
            assert "density_kg_m3" in mat, f"{mat.get('name')} missing density"

    def test_density_values_are_positive(self) -> None:
        for mat in self._materials():
            d = mat.get("density_kg_m3", 0)
            if d is not None:
                assert float(d) > 0, f"{mat.get('name')} density <= 0"

    def test_brass_has_youngs_modulus(self) -> None:
        by_name = {m["name"]: m for m in self._materials()}
        assert "youngs_modulus_GPa" in by_name["brass"]

    def test_cast_iron_present(self) -> None:
        names = [m["name"] for m in self._materials()]
        assert "cast_iron" in names

    def test_steel_present(self) -> None:
        names = [m["name"] for m in self._materials()]
        assert "steel" in names

    def test_mercury_present(self) -> None:
        names = [m["name"] for m in self._materials()]
        assert "mercury" in names

    def test_organic_fiber_present(self) -> None:
        names = [m["name"] for m in self._materials()]
        assert "organic_fiber" in names

    def test_no_duplicate_material_names(self) -> None:
        names = [m["name"] for m in self._materials()]
        assert len(names) == len(set(names)), "Duplicate material names"


class TestSimSchemaConfigValues:
    """Verify config section values are physically plausible."""

    def _schema(self) -> dict:
        return yaml.safe_load(SIM_SCHEMA.read_text(encoding="utf-8"))

    def test_gear_module_is_fractional_mm(self) -> None:
        val = self._schema()["tolerances"]["gear_module_mm"]
        assert 0.1 <= val <= 5.0, f"gear_module_mm out of range: {val}"

    def test_backlash_is_small_positive(self) -> None:
        val = self._schema()["tolerances"]["backlash_mm"]
        assert 0 <= val < 0.5

    def test_lubrication_viscosity_realistic(self) -> None:
        val = self._schema()["lubrication"]["viscosity_cSt_40C"]
        assert 1 <= val <= 1000, f"viscosity out of range: {val}"

    def test_card_feed_rate_realistic(self) -> None:
        val = self._schema()["mechanisms"]["card_feed"]["feed_rate_cards_per_min"]
        assert 10 <= val <= 1000, f"feed_rate out of plausible range: {val}"

    def test_cutoff_pct_in_0_to_100(self) -> None:
        val = self._schema()["valve_gear_params"]["cutoff_pct"]
        assert 0 < val <= 100

    def test_jam_rate_non_negative(self) -> None:
        val = self._schema()["mechanisms"]["card_feed"]["jam_rate_per_1k"]
        assert val >= 0


class TestSimSchemaNewMaterials:
    """Verify all 7 new historical materials are present in MaterialLibrary."""

    def test_corinthian_bronze_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "corinthian_bronze" in lib.names()

    def test_ivory_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "ivory" in lib.names()

    def test_boxwood_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "boxwood" in lib.names()

    def test_aluminum_alloy_1940s_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "aluminum_alloy_1940s" in lib.names()

    def test_bakelite_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "bakelite" in lib.names()

    def test_organic_fiber_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "organic_fiber" in lib.names()

    def test_mercury_loaded(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert "mercury" in lib.names()

    def test_total_material_count_at_least_fourteen(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        assert len(lib.all_materials()) >= 14


class TestSimSchemaCoreProperties:
    """Core material property range checks for the original materials."""

    def test_steel_density_range(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        steel = lib.get("steel")
        assert 7700 <= steel.density_kg_m3 <= 8050

    def test_brass_youngs_modulus_range(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        brass = lib.get("brass")
        e_min, e_max = brass.youngs_modulus_Pa
        assert 85e9 <= e_min <= 115e9

    def test_cast_iron_density_range(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        ci = lib.get("cast_iron")
        assert 7000 <= ci.density_kg_m3 <= 7400

    def test_phosphor_bronze_density_positive(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        pb = lib.get("phosphor_bronze")
        assert pb.density_kg_m3 > 0

    def test_all_materials_have_positive_density(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        for mat in lib.all_materials():
            assert mat.density_kg_m3 > 0, f"{mat.name} has non-positive density"

    def test_all_structural_materials_have_youngs_modulus_tuple(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        non_structural = {"organic_fiber", "mercury"}
        for mat in lib.all_materials():
            if mat.name not in non_structural:
                e_min, e_max = mat.youngs_modulus_Pa
                assert e_min > 0, f"{mat.name} has non-positive E_min"
