"""Tests for operational envelope sweep tool output format and physics."""

import pytest

pytestmark = pytest.mark.physics

from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig


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
