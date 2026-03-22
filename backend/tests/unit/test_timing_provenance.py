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


class TestTimingYAMLValueRanges:
    """Verify TIMING_PROVISIONAL.yaml parameter values are physically plausible."""

    def _cfg(self) -> dict:
        with open(TIMING_YAML) as f:
            return yaml.safe_load(f)

    def test_lap_mm_within_plausible_range(self) -> None:
        lap = self._cfg()["valve_gear"]["parameters"]["lap_mm"]["value"]
        assert 0.0 < lap < 20.0

    def test_lead_mm_within_plausible_range(self) -> None:
        lead = self._cfg()["valve_gear"]["parameters"]["lead_mm"]["value"]
        assert 0.0 < lead < 10.0

    def test_cutoff_pct_is_a_valid_percentage(self) -> None:
        cutoff = self._cfg()["valve_gear"]["parameters"]["cutoff_pct"]["value"]
        assert 0.0 < cutoff <= 100.0

    def test_source_strings_are_non_empty(self) -> None:
        vg = self._cfg()["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            source = vg[key].get("source", "")
            assert len(source) > 0, f"{key} source is empty"

    def test_all_param_values_are_numeric(self) -> None:
        vg = self._cfg()["valve_gear"]["parameters"]
        for key in ("lap_mm", "lead_mm", "cutoff_pct"):
            value = vg[key]["value"]
            assert isinstance(value, (int, float)), f"{key} value is not numeric"

    def test_lap_mm_value_is_numeric_type(self) -> None:
        lap = self._cfg()["valve_gear"]["parameters"]["lap_mm"]["value"]
        assert isinstance(lap, (int, float))

    def test_lead_smaller_than_lap(self) -> None:
        vg = self._cfg()["valve_gear"]["parameters"]
        lap = vg["lap_mm"]["value"]
        lead = vg["lead_mm"]["value"]
        # Historical AE steam: lead < lap is the expected relation
        assert lead < lap


class TestSimulationConfigFieldTypes:
    """SimulationConfig field type and override tests."""

    def test_valve_lap_mm_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.valve_lap_mm, (int, float))

    def test_valve_lead_mm_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.valve_lead_mm, (int, float))

    def test_valve_cutoff_pct_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.valve_cutoff_pct, (int, float))

    def test_steam_pressure_bar_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.steam_pressure_bar, (int, float))

    def test_piston_stroke_m_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.piston_stroke_m, (int, float))

    def test_thermal_efficiency_pct_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.thermal_efficiency_pct, (int, float))

    def test_all_three_valve_fields_override_together(self) -> None:
        cfg = SimulationConfig(valve_lap_mm=5.0, valve_lead_mm=2.0, valve_cutoff_pct=55.0)
        assert cfg.valve_lap_mm == 5.0
        assert cfg.valve_lead_mm == 2.0
        assert cfg.valve_cutoff_pct == 55.0


class TestSimulationConfigValveParameters:
    """Valve timing parameter validation and construction."""

    def test_valve_lap_default_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.valve_lap_mm, (int, float))

    def test_valve_lead_default_is_numeric(self) -> None:
        cfg = SimulationConfig()
        assert isinstance(cfg.valve_lead_mm, (int, float))

    def test_custom_rpm_overrides_default(self) -> None:
        cfg = SimulationConfig(rpm=60.0)
        assert cfg.rpm == 60.0

    def test_ambient_temperature_default_is_20(self) -> None:
        cfg = SimulationConfig()
        assert cfg.ambient_temperature_C == pytest.approx(20.0)

    def test_dt_s_default_positive(self) -> None:
        cfg = SimulationConfig()
        assert cfg.dt_s > 0

    def test_config_equality_same_defaults(self) -> None:
        c1 = SimulationConfig()
        c2 = SimulationConfig()
        assert c1 == c2

    def test_config_inequality_different_rpm(self) -> None:
        c1 = SimulationConfig(rpm=30.0)
        c2 = SimulationConfig(rpm=60.0)
        assert c1 != c2


class TestTimingYamlStructure:
    """TIMING_PROVISIONAL.yaml structural validation."""

    def _load(self) -> dict:
        import yaml
        with open(TIMING_YAML) as f:
            return yaml.safe_load(f)

    def test_yaml_has_engine_key_or_is_dict(self) -> None:
        data = self._load()
        assert isinstance(data, dict)

    def test_yaml_non_empty(self) -> None:
        data = self._load()
        assert len(data) > 0

    def test_yaml_values_are_not_all_null(self) -> None:
        data = self._load()
        # At least one value should be non-None
        all_vals = list(data.values())
        assert any(v is not None for v in all_vals)


class TestSimulationConfigExtended:
    """SimulationConfig additional field defaults."""

    def test_temperature_limit_is_positive(self) -> None:
        from backend.src.emulator.simulation.state import SimulationConfig
        cfg = SimulationConfig()
        assert cfg.temperature_limit_C > 0

    def test_rpm_default_positive(self) -> None:
        from backend.src.emulator.simulation.state import SimulationConfig
        cfg = SimulationConfig()
        assert cfg.rpm > 0

    def test_machine_mass_default_positive(self) -> None:
        from backend.src.emulator.simulation.state import SimulationConfig
        cfg = SimulationConfig()
        assert cfg.machine_mass_kg > 0

    def test_surface_area_default_positive(self) -> None:
        from backend.src.emulator.simulation.state import SimulationConfig
        cfg = SimulationConfig()
        assert cfg.surface_area_m2 > 0
