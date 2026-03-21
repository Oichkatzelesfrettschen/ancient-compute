"""Tests for operational envelope sweep tool output format and physics."""

import pytest

from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig

pytestmark = pytest.mark.physics


class TestOperationalEnvelope:
    """Verify operational envelope sweep produces expected results."""

    def test_single_rpm_produces_result(self):
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        result = engine.predict_maintenance(max_hours=0.1)
        assert result.final_state.temperature_C > 20.0
        assert result.steps > 0

    def test_higher_rpm_higher_temperature(self):
        """Higher RPM should produce higher steady-state temperature."""
        temps = []
        for rpm in [20.0, 60.0, 100.0]:
            config = SimulationConfig(rpm=rpm)
            engine = SimulationEngine(config)
            result = engine.predict_maintenance(max_hours=0.5)
            temps.append(result.final_state.temperature_C)
        assert temps[0] < temps[1] < temps[2], f"Temps not monotonic: {temps}"

    def test_higher_rpm_higher_wear(self):
        """Higher RPM should produce more bearing wear."""
        wears = []
        for rpm in [20.0, 60.0, 100.0]:
            config = SimulationConfig(rpm=rpm)
            engine = SimulationEngine(config)
            result = engine.predict_maintenance(max_hours=0.5)
            max_wear = 0.0
            if result.final_state.bearing_wear_volumes_mm3:
                max_wear = max(result.final_state.bearing_wear_volumes_mm3)
            wears.append(max_wear)
        assert wears[0] < wears[1] < wears[2], f"Wear not monotonic: {wears}"

    def test_result_fields_present(self):
        """Verify all expected fields exist on the result."""
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        result = engine.predict_maintenance(max_hours=0.1)
        assert hasattr(result.final_state, "temperature_C")
        assert hasattr(result.final_state, "bearing_wear_volumes_mm3")
        assert hasattr(result.final_state, "shaft_deflection_mm")
        assert hasattr(result.final_state, "gear_backlash_mm")
        assert hasattr(result.final_state, "lubrication_regime")
        assert hasattr(result.final_state, "energy_consumed_J")
        assert hasattr(result, "failure_time_s")
        assert hasattr(result, "limiting_component")
        assert hasattr(result, "steps")

    def test_low_rpm_no_failure(self):
        """At low RPM with short duration, no failure should occur."""
        config = SimulationConfig(rpm=10.0)
        engine = SimulationEngine(config)
        result = engine.predict_maintenance(max_hours=0.1)
        assert result.failure_time_s == float("inf") or result.failure_time_s is None
        assert not result.limiting_component or result.limiting_component == "none"


class TestSimulationResultFields:
    def test_duration_s_positive(self) -> None:
        config = SimulationConfig(rpm=30.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.1)
        assert result.duration_s > 0

    def test_history_is_list(self) -> None:
        config = SimulationConfig(rpm=30.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.05)
        assert isinstance(result.history, list)

    def test_history_is_non_empty(self) -> None:
        config = SimulationConfig(rpm=30.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.05)
        assert len(result.history) >= 1

    def test_final_state_temperature_above_ambient(self) -> None:
        config = SimulationConfig(rpm=60.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.5)
        # Friction heating should raise temp above 20 C
        assert result.final_state.temperature_C > 20.0

    def test_gear_backlash_non_negative(self) -> None:
        config = SimulationConfig(rpm=30.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.1)
        assert result.final_state.gear_backlash_mm >= 0.0

    def test_energy_consumed_non_negative(self) -> None:
        config = SimulationConfig(rpm=30.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.1)
        assert result.final_state.energy_consumed_J >= 0.0

    def test_bearing_wear_list_is_non_empty(self) -> None:
        config = SimulationConfig(rpm=60.0)
        result = SimulationEngine(config).predict_maintenance(max_hours=0.1)
        assert len(result.final_state.bearing_wear_volumes_mm3) > 0

    def test_longer_run_more_wear(self) -> None:
        config_short = SimulationConfig(rpm=60.0)
        config_long = SimulationConfig(rpm=60.0)
        r_short = SimulationEngine(config_short).predict_maintenance(max_hours=0.1)
        r_long = SimulationEngine(config_long).predict_maintenance(max_hours=1.0)
        max_wear_short = max(r_short.final_state.bearing_wear_volumes_mm3 or [0.0])
        max_wear_long = max(r_long.final_state.bearing_wear_volumes_mm3 or [0.0])
        assert max_wear_long >= max_wear_short
