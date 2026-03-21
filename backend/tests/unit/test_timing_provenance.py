"""Tests for valve timing provenance and configuration loading."""

from pathlib import Path

import pytest
import yaml

from backend.src.emulator.simulation.state import SimulationConfig

pytestmark = pytest.mark.physics

TIMING_YAML = (
    Path(__file__).parent.parent.parent.parent / "docs" / "simulation" / "TIMING_PROVISIONAL.yaml"
)


class TestTimingProvenance:
    """Verify timing YAML exists, is well-formed, and matches SimulationConfig."""

    def test_yaml_loads(self):
        assert TIMING_YAML.exists(), f"Missing {TIMING_YAML}"
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        assert isinstance(config, dict)

    def test_valve_gear_params_present(self):
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        vg = config["valve_gear"]["parameters"]
        assert "lap_mm" in vg
        assert "lead_mm" in vg
        assert "cutoff_pct" in vg

    def test_all_params_have_source(self):
        """Every parameter with 'provisional' must also have 'source'."""
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        missing = []
        _walk_for_sources(config, "", missing)
        assert missing == [], f"Parameters missing source: {missing}"

    def test_provisional_params_flagged(self):
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        vg = config["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            assert vg[key]["provisional"] is True, f"{key} should be provisional"

    def test_cutoff_range(self):
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        cutoff = config["valve_gear"]["parameters"]["cutoff_pct"]["value"]
        assert 0 < cutoff <= 100


class TestSimulationConfigValveTiming:
    """Verify SimulationConfig includes valve timing fields."""

    def test_default_valve_params(self):
        cfg = SimulationConfig()
        assert cfg.valve_lap_mm == 3.0
        assert cfg.valve_lead_mm == 1.0
        assert cfg.valve_cutoff_pct == 60.0

    def test_default_steam_params(self):
        cfg = SimulationConfig()
        assert cfg.steam_pressure_bar == 6.0
        assert cfg.piston_stroke_m == 0.2
        assert cfg.thermal_efficiency_pct == 8.0

    def test_custom_valve_params(self):
        cfg = SimulationConfig(
            valve_lap_mm=4.0,
            valve_lead_mm=1.5,
            valve_cutoff_pct=50.0,
        )
        assert cfg.valve_lap_mm == 4.0
        assert cfg.valve_lead_mm == 1.5
        assert cfg.valve_cutoff_pct == 50.0

    def test_yaml_values_match_defaults(self):
        """TIMING_PROVISIONAL.yaml values must match SimulationConfig defaults."""
        with open(TIMING_YAML) as f:
            config = yaml.safe_load(f)
        vg = config["valve_gear"]["parameters"]
        cfg = SimulationConfig()
        assert cfg.valve_lap_mm == vg["lap_mm"]["value"]
        assert cfg.valve_lead_mm == vg["lead_mm"]["value"]
        assert cfg.valve_cutoff_pct == vg["cutoff_pct"]["value"]


def _walk_for_sources(obj, path, missing):
    """Recursively find parameters with 'provisional' but no 'source'."""
    if isinstance(obj, dict):
        if "provisional" in obj and "value" in obj:
            if not obj.get("source"):
                missing.append(path)
        else:
            for key, val in obj.items():
                _walk_for_sources(val, f"{path}.{key}" if path else key, missing)


class TestTimingYAMLStructure:
    """Low-level structure checks on TIMING_PROVISIONAL.yaml."""

    def _cfg(self) -> dict:
        import yaml

        with open(TIMING_YAML) as f:
            return yaml.safe_load(f)

    def test_top_level_valve_gear_key_present(self) -> None:
        cfg = self._cfg()
        assert "valve_gear" in cfg

    def test_valve_gear_has_parameters_key(self) -> None:
        cfg = self._cfg()
        assert "parameters" in cfg["valve_gear"]

    def test_lap_mm_value_is_positive(self) -> None:
        cfg = self._cfg()
        lap = cfg["valve_gear"]["parameters"]["lap_mm"]["value"]
        assert lap > 0

    def test_lead_mm_value_is_positive(self) -> None:
        cfg = self._cfg()
        lead = cfg["valve_gear"]["parameters"]["lead_mm"]["value"]
        assert lead > 0

    def test_cutoff_pct_between_0_and_100(self) -> None:
        cfg = self._cfg()
        cutoff = cfg["valve_gear"]["parameters"]["cutoff_pct"]["value"]
        assert 0 < cutoff <= 100

    def test_all_three_params_have_source_key(self) -> None:
        cfg = self._cfg()
        vg = cfg["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            assert "source" in vg[key], f"{key} missing source"

    def test_all_three_params_have_value_key(self) -> None:
        cfg = self._cfg()
        vg = cfg["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            assert "value" in vg[key], f"{key} missing value"

    def test_all_three_params_have_provisional_true(self) -> None:
        cfg = self._cfg()
        vg = cfg["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            assert vg[key]["provisional"] is True, f"{key} should be provisional"


class TestSimulationConfigEdgeCases:
    """SimulationConfig boundary and custom value tests."""

    def test_custom_steam_pressure(self) -> None:
        cfg = SimulationConfig(steam_pressure_bar=10.0)
        assert cfg.steam_pressure_bar == 10.0

    def test_custom_piston_stroke(self) -> None:
        cfg = SimulationConfig(piston_stroke_m=0.5)
        assert cfg.piston_stroke_m == 0.5

    def test_custom_thermal_efficiency(self) -> None:
        cfg = SimulationConfig(thermal_efficiency_pct=12.0)
        assert cfg.thermal_efficiency_pct == 12.0

    def test_valve_params_independent_of_steam_params(self) -> None:
        """Valve and steam fields are independent SimulationConfig fields."""
        cfg = SimulationConfig(valve_lap_mm=5.0, steam_pressure_bar=8.0)
        assert cfg.valve_lap_mm == 5.0
        assert cfg.steam_pressure_bar == 8.0

    def test_all_default_fields_are_positive(self) -> None:
        cfg = SimulationConfig()
        assert cfg.valve_lap_mm > 0
        assert cfg.valve_lead_mm > 0
        assert cfg.valve_cutoff_pct > 0
        assert cfg.steam_pressure_bar > 0
        assert cfg.piston_stroke_m > 0
        assert cfg.thermal_efficiency_pct > 0
