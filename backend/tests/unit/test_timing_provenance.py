"""Tests for valve timing provenance and configuration loading."""

import pytest
from pathlib import Path

import yaml

from backend.src.emulator.simulation.state import SimulationConfig


TIMING_YAML = (
    Path(__file__).parent.parent.parent.parent
    / "docs"
    / "simulation"
    / "TIMING_PROVISIONAL.yaml"
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
