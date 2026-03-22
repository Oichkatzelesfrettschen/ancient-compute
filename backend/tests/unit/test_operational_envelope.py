"""Tests for operational envelope sweep tool output format and physics."""

import pytest

from backend.src.emulator.simulation.engine import SimulationEngine, StepResult
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


class TestStepResult:
    """StepResult fields returned by a single step()."""

    def _step_once(self, rpm: float = 30.0) -> StepResult:
        config = SimulationConfig(rpm=rpm)
        engine = SimulationEngine(config)
        return engine.step()

    def test_step_returns_step_result(self) -> None:
        result = self._step_once()
        assert isinstance(result, StepResult)

    def test_step_time_positive(self) -> None:
        result = self._step_once()
        assert result.time_s > 0.0

    def test_step_temperature_is_float(self) -> None:
        result = self._step_once()
        assert isinstance(result.temperature_C, float)

    def test_step_max_clearance_non_negative(self) -> None:
        result = self._step_once()
        assert result.max_clearance_mm >= 0.0

    def test_step_max_wear_non_negative(self) -> None:
        result = self._step_once()
        assert result.max_wear_volume_mm3 >= 0.0

    def test_step_shaft_deflection_non_negative(self) -> None:
        result = self._step_once()
        assert result.shaft_deflection_mm >= 0.0

    def test_step_total_heat_positive(self) -> None:
        result = self._step_once()
        assert result.total_heat_W > 0.0

    def test_step_friction_coeff_in_range(self) -> None:
        result = self._step_once()
        assert 0.0 < result.friction_coeff <= 1.0

    def test_step_lubrication_regime_is_string(self) -> None:
        result = self._step_once()
        assert isinstance(result.lubrication_regime, str)
        assert result.lubrication_regime in {"full_film", "mixed", "boundary"}

    def test_step_lambda_ratio_positive(self) -> None:
        result = self._step_once()
        assert result.lambda_ratio > 0.0


class TestSimulationEngineProperties:
    """failed / failure_reason properties and reset()."""

    def test_initial_not_failed(self) -> None:
        engine = SimulationEngine(SimulationConfig(rpm=30.0))
        assert engine.failed is False

    def test_initial_failure_reason_empty(self) -> None:
        engine = SimulationEngine(SimulationConfig(rpm=30.0))
        assert engine.failure_reason == ""

    def test_reset_clears_failed(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        # Force a failure via bearing seizure
        engine.state.bearing_clearances_mm = [0.0, 0.05, 0.05, 0.05]
        engine._check_failures()
        assert engine.failed
        engine.reset()
        assert engine.failed is False

    def test_reset_clears_failure_reason(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        engine.state.bearing_clearances_mm = [0.0, 0.05, 0.05, 0.05]
        engine._check_failures()
        engine.reset()
        assert engine.failure_reason == ""

    def test_reset_restores_time_to_zero(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        engine.run(10.0)
        engine.reset()
        assert engine.state.time_s == 0.0

    def test_temperature_failure_triggers_failed(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        engine.state.temperature_C = config.temperature_limit_C + 1.0
        engine._check_failures()
        assert engine.failed
        assert engine.failure_reason == "temperature"

    def test_bearing_seizure_triggers_failed(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        engine.state.bearing_clearances_mm = [0.0, 0.1, 0.1, 0.1]
        engine._check_failures()
        assert engine.failed
        assert engine.failure_reason == "bearing_seizure"


class TestSimulationStateFields:
    """Verify SimulationState initial values and copy()."""

    def test_initial_temperature_is_ambient(self) -> None:
        config = SimulationConfig(ambient_temperature_C=25.0)
        engine = SimulationEngine(config)
        assert engine.state.temperature_C == pytest.approx(25.0)

    def test_initial_time_zero(self) -> None:
        engine = SimulationEngine(SimulationConfig())
        assert engine.state.time_s == 0.0

    def test_initial_shaft_angle_zero(self) -> None:
        engine = SimulationEngine(SimulationConfig())
        assert engine.state.shaft_angle_deg == 0.0

    def test_initial_oil_age_zero(self) -> None:
        engine = SimulationEngine(SimulationConfig())
        assert engine.state.oil_age_hours == 0.0

    def test_initial_fatigue_damage_zero(self) -> None:
        engine = SimulationEngine(SimulationConfig())
        assert engine.state.cumulative_fatigue_damage == 0.0

    def test_copy_is_independent(self) -> None:
        state = SimulationEngine(SimulationConfig()).state
        copy = state.copy()
        copy.temperature_C = 9999.0
        assert state.temperature_C != 9999.0

    def test_step_advances_shaft_angle(self) -> None:
        # 30 RPM: 180 deg/s with dt=1s -> shaft_angle becomes 180
        engine = SimulationEngine(SimulationConfig(rpm=30.0))
        engine.step()
        assert engine.state.shaft_angle_deg == pytest.approx(180.0)

    def test_run_accumulates_sliding_distance(self) -> None:
        # Each step advances sliding distance; non-zero after run
        config = SimulationConfig(rpm=60.0)
        engine = SimulationEngine(config)
        engine.run(100.0)
        assert engine.state.total_sliding_distance_mm > 0.0

    def test_oil_age_accumulates_during_run(self) -> None:
        config = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(config)
        engine.run(3600.0)  # 1 hour
        assert engine.state.oil_age_hours > 0.0


class TestSimulationStepResultExtra:
    """Additional StepResult field type and range tests."""

    def _step(self) -> StepResult:
        cfg = SimulationConfig(rpm=30.0)
        return SimulationEngine(cfg).step()

    def test_time_s_is_float(self) -> None:
        assert isinstance(self._step().time_s, float)

    def test_temperature_is_float(self) -> None:
        assert isinstance(self._step().temperature_C, float)

    def test_shaft_deflection_is_float(self) -> None:
        assert isinstance(self._step().shaft_deflection_mm, float)

    def test_friction_coeff_is_float(self) -> None:
        assert isinstance(self._step().friction_coeff, float)

    def test_lubrication_regime_is_str(self) -> None:
        assert isinstance(self._step().lubrication_regime, str)

    def test_lambda_ratio_nonneg(self) -> None:
        assert self._step().lambda_ratio >= 0.0

    def test_wear_volume_nonneg(self) -> None:
        assert self._step().max_wear_volume_mm3 >= 0.0

    def test_temperature_gt_zero(self) -> None:
        assert self._step().temperature_C > 0.0

    def test_shaft_deflection_nonneg(self) -> None:
        assert self._step().shaft_deflection_mm >= 0.0


class TestSimulationEngineRunExtra:
    """Additional SimulationEngine.run() result tests."""

    def test_run_returns_sim_result(self) -> None:
        from backend.src.emulator.simulation.engine import SimulationResult
        cfg = SimulationConfig(rpm=30.0)
        result = SimulationEngine(cfg).run(10.0)
        assert isinstance(result, SimulationResult)

    def test_run_10s_records_produced(self) -> None:
        from backend.src.emulator.simulation.engine import SimulationResult
        cfg = SimulationConfig(rpm=30.0)
        result = SimulationEngine(cfg).run(120.0, record_interval_s=60.0)
        assert hasattr(result, "history")

    def test_run_oil_age_positive(self) -> None:
        cfg = SimulationConfig(rpm=30.0)
        engine = SimulationEngine(cfg)
        engine.run(7200.0)  # 2 hours
        assert engine.state.oil_age_hours > 0.0
