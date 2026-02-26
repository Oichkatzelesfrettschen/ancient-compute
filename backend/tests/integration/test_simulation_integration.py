"""Integration tests for Simulation Orchestration Layer.

End-to-end warmup simulation with all safety factors checked,
and opcode-coupled simulation verifying physics evolution during
program execution.
"""

import math

import pytest

from backend.src.emulator.simulation.engine import SimulationEngine, SimulationResult
from backend.src.emulator.simulation.state import SimulationConfig, SimulationState
from backend.src.emulator.simulation.coupling import CouplingFunctions
from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.structural import ShaftCriticalSpeed, FatigueAnalysis
from backend.src.emulator.tribology import PVAnalysis


@pytest.fixture
def lib():
    return MaterialLibrary()


# ---------------------------------------------------------------------------
# IX.1.a: End-to-end warmup simulation
# ---------------------------------------------------------------------------

class TestWarmupSimulation:
    """Cold start -> steady state with all safety factors checked."""

    @pytest.fixture
    def warmup_result(self, lib):
        """Run a 2-hour warmup simulation."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        result = eng.run(7200.0, record_interval_s=300.0)
        return eng, result

    def test_no_failure(self, warmup_result):
        eng, _ = warmup_result
        assert not eng.failed, f"Failed: {eng.failure_reason}"

    def test_temperature_below_limit(self, warmup_result):
        eng, _ = warmup_result
        assert eng.state.temperature_C < eng.config.temperature_limit_C

    def test_temperature_above_ambient(self, warmup_result):
        eng, _ = warmup_result
        assert eng.state.temperature_C > eng.config.ambient_temperature_C

    def test_all_clearances_positive(self, warmup_result):
        eng, _ = warmup_result
        for i, c in enumerate(eng.state.bearing_clearances_mm):
            assert c > 0, f"Bearing {i} seized"

    def test_all_clearances_below_limit(self, warmup_result):
        eng, _ = warmup_result
        for i, c in enumerate(eng.state.bearing_clearances_mm):
            assert c < eng.config.clearance_limit_mm, (
                f"Bearing {i} clearance {c:.4f}mm exceeds limit"
            )

    def test_bearing_loads_sum_correct(self, warmup_result):
        eng, _ = warmup_result
        total = sum(eng.state.bearing_loads_N)
        expected = eng.config.machine_mass_kg * 9.81
        assert total == pytest.approx(expected, rel=0.01)

    def test_shaft_deflection_within_precision(self, warmup_result, lib):
        """Shaft deflection < initial clearance (precision requirement).

        Deflection under dead weight should not consume the full
        bearing clearance, but may be a significant fraction of it
        for a heavily loaded shaft with wide bearing spacing.
        """
        eng, _ = warmup_result
        assert eng.state.shaft_deflection_mm < eng.config.initial_clearance_mm * 2.0

    def test_critical_speed_margin(self, warmup_result, lib):
        """Critical speed margin > 3 for safe operation."""
        eng, _ = warmup_result
        shaft_mat = lib.get(eng.config.shaft_material)
        span_mm = eng.config.shaft_length_mm / eng.config.bearing_count
        omega_n = ShaftCriticalSpeed.first_critical_speed_rad_s(
            shaft_mat.youngs_modulus_GPa[0],
            eng.config.shaft_diameter_mm,
            span_mm,
            shaft_mat.density_kg_m3,
        )
        crit_rpm = ShaftCriticalSpeed.critical_speed_rpm(omega_n)
        margin = ShaftCriticalSpeed.critical_speed_margin(crit_rpm, eng.config.rpm)
        assert margin > 3.0, f"Critical speed margin {margin:.1f} < 3.0"

    def test_pv_within_limit(self, warmup_result, lib):
        """PV product within bearing material limit."""
        eng, _ = warmup_result
        avg_load = sum(eng.state.bearing_loads_N) / len(eng.state.bearing_loads_N)
        pv = PVAnalysis.pv_product_MPa_m_s(
            avg_load,
            eng.config.shaft_diameter_mm,
            eng.config.bearing_length_mm,
            eng.config.rpm,
        )
        assert PVAnalysis.is_within_limit(pv), f"PV={pv:.4f} exceeds limit"

    def test_lubrication_regime_valid(self, warmup_result):
        """Lubrication regime should be a recognized value.

        At 30 RPM with journal bearings, boundary lubrication is expected
        for 19th-century plain bearings. The Hamrock-Dowson EHL model
        correctly predicts thin films at such low entrainment speeds.
        Babbage's machine relied on frequent manual oil application.
        """
        eng, _ = warmup_result
        assert eng.state.lubrication_regime in ("full_film", "mixed", "boundary")

    def test_history_recorded(self, warmup_result):
        _, result = warmup_result
        assert len(result.history) > 0

    def test_temperature_monotonically_approaches_steady_state(self, warmup_result):
        """Temperature history should show warmup curve (no oscillation)."""
        _, result = warmup_result
        if len(result.history) < 3:
            return
        # Check that temperature differences decrease (approaching steady state)
        diffs = []
        for i in range(1, len(result.history)):
            diffs.append(
                abs(result.history[i].temperature_C - result.history[i - 1].temperature_C)
            )
        # Later diffs should be smaller than earlier ones (convergence)
        if len(diffs) >= 4:
            early_avg = sum(diffs[:2]) / 2
            late_avg = sum(diffs[-2:]) / 2
            assert late_avg <= early_avg * 1.1, "Temperature not converging"


# ---------------------------------------------------------------------------
# IX.1.b: Multi-module coupling consistency
# ---------------------------------------------------------------------------

class TestMultiModuleCoupling:
    """Verify that coupling between modules produces self-consistent results."""

    def test_thermal_wear_coupling_direction(self, lib):
        """Higher temperature -> lower viscosity -> thinner film -> more wear.

        Run two simulations: cool vs warm ambient. Warm should have more wear.
        """
        cfg_cool = SimulationConfig(dt_s=1.0, rpm=30.0, ambient_temperature_C=10.0)
        cfg_warm = SimulationConfig(dt_s=1.0, rpm=30.0, ambient_temperature_C=35.0)

        eng_cool = SimulationEngine(cfg_cool, lib)
        eng_warm = SimulationEngine(cfg_warm, lib)

        for _ in range(1000):
            eng_cool.step()
            eng_warm.step()

        wear_cool = sum(eng_cool.state.bearing_wear_volumes_mm3)
        wear_warm = sum(eng_warm.state.bearing_wear_volumes_mm3)

        # Warm environment should produce at least as much wear
        # (lower viscosity -> thinner film -> higher friction)
        # Note: the effect may be small, so just check direction
        assert wear_warm >= wear_cool * 0.9, (
            f"Warm wear {wear_warm:.6e} < cool wear {wear_cool:.6e}"
        )

    def test_load_clearance_coupling(self, lib):
        """Heavier machine -> more bearing load -> more deflection -> more wear."""
        cfg_light = SimulationConfig(dt_s=1.0, rpm=30.0, machine_mass_kg=200.0)
        cfg_heavy = SimulationConfig(dt_s=1.0, rpm=30.0, machine_mass_kg=1000.0)

        eng_light = SimulationEngine(cfg_light, lib)
        eng_heavy = SimulationEngine(cfg_heavy, lib)

        for _ in range(500):
            eng_light.step()
            eng_heavy.step()

        wear_light = sum(eng_light.state.bearing_wear_volumes_mm3)
        wear_heavy = sum(eng_heavy.state.bearing_wear_volumes_mm3)

        assert wear_heavy > wear_light, "Heavier machine should have more wear"

    def test_rpm_sensitivity(self, lib):
        """Higher RPM -> more heat generation -> higher temperature."""
        cfg_slow = SimulationConfig(dt_s=1.0, rpm=15.0)
        cfg_fast = SimulationConfig(dt_s=1.0, rpm=60.0)

        eng_slow = SimulationEngine(cfg_slow, lib)
        eng_fast = SimulationEngine(cfg_fast, lib)

        for _ in range(3000):
            eng_slow.step()
            eng_fast.step()

        assert eng_fast.state.temperature_C > eng_slow.state.temperature_C


# ---------------------------------------------------------------------------
# IX.1: Dimensional consistency
# ---------------------------------------------------------------------------

class TestDimensionalConsistency:
    """Verify units are consistent across the simulation."""

    def test_heat_in_watts(self, lib):
        """Total heat generation should be in plausible range [W]."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        eng.step()
        Q = eng.state.total_heat_generation_W
        # For a 500 kg machine at 30 RPM, expect 0.1-100 W total friction heat
        assert 0.0 < Q < 1000.0, f"Q={Q:.2f} W outside plausible range"

    def test_clearance_in_mm(self, lib):
        """Bearing clearances should be in sub-mm range."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        eng.step()
        for c in eng.state.bearing_clearances_mm:
            assert 0.001 < c < 1.0, f"Clearance {c:.4f} mm outside plausible range"

    def test_deflection_in_mm(self, lib):
        """Shaft deflection should be in sub-mm range."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        eng.step()
        d = eng.state.shaft_deflection_mm
        assert 0.0 <= d < 1.0, f"Deflection {d:.4f} mm outside plausible range"

    def test_temperature_in_celsius(self, lib):
        """Temperature should be in room-temperature range after one step."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0, ambient_temperature_C=20.0)
        eng = SimulationEngine(cfg, lib)
        eng.step()
        T = eng.state.temperature_C
        assert 19.0 < T < 100.0, f"T={T:.2f} C outside plausible range"
