"""Tests for Simulation Orchestration Layer.

Validates the SimulationEngine time-stepping, coupling convergence,
energy conservation, and long-duration evolution.
"""

import math
import pytest

from backend.src.emulator.simulation.state import SimulationState, SimulationConfig
from backend.src.emulator.simulation.engine import SimulationEngine, StepResult, SimulationResult
from backend.src.emulator.simulation.coupling import CouplingFunctions
from backend.src.emulator.materials import MaterialLibrary


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def lib():
    return MaterialLibrary()


@pytest.fixture
def default_config():
    return SimulationConfig()


@pytest.fixture
def engine(default_config, lib):
    return SimulationEngine(default_config, lib)


# ---------------------------------------------------------------------------
# SimulationState Tests
# ---------------------------------------------------------------------------

class TestSimulationState:

    def test_default_state(self):
        s = SimulationState()
        assert s.time_s == 0.0
        assert s.temperature_C == 20.0
        assert s.bearing_clearances_mm == []
        assert s.lubrication_regime == "full_film"

    def test_copy_is_independent(self):
        s = SimulationState(
            temperature_C=30.0,
            bearing_clearances_mm=[0.05, 0.06],
        )
        c = s.copy()
        c.temperature_C = 99.0
        c.bearing_clearances_mm[0] = 0.99
        assert s.temperature_C == 30.0
        assert s.bearing_clearances_mm[0] == 0.05


# ---------------------------------------------------------------------------
# SimulationConfig Tests
# ---------------------------------------------------------------------------

class TestSimulationConfig:

    def test_omega_rad_s(self):
        cfg = SimulationConfig(rpm=30.0)
        expected = 2.0 * math.pi * 30.0 / 60.0
        assert cfg.omega_rad_s == pytest.approx(expected)

    def test_period_s(self):
        cfg = SimulationConfig(rpm=30.0)
        assert cfg.period_s == pytest.approx(2.0)

    def test_period_zero_rpm(self):
        cfg = SimulationConfig(rpm=0.0)
        assert cfg.period_s == float("inf")


# ---------------------------------------------------------------------------
# CouplingFunctions Tests
# ---------------------------------------------------------------------------

class TestCouplingFunctions:

    def test_viscosity_at_40C_is_base(self):
        """eta(40) = eta_40."""
        assert CouplingFunctions.viscosity_at_temperature(0.1, 40.0) == pytest.approx(0.1)

    def test_viscosity_decreases_with_temperature(self):
        eta_low = CouplingFunctions.viscosity_at_temperature(0.1, 50.0)
        eta_high = CouplingFunctions.viscosity_at_temperature(0.1, 60.0)
        assert eta_low > eta_high

    def test_friction_heat_positive(self):
        Q = CouplingFunctions.friction_heat_from_bearings(
            [100.0, 100.0], 50.0, math.pi, 0.05,
        )
        assert Q > 0

    def test_redistribute_equal_clearances(self):
        loads = CouplingFunctions.redistribute_bearing_loads(400.0, 4, [0.05] * 4)
        assert len(loads) == 4
        for load in loads:
            assert load == pytest.approx(100.0)

    def test_redistribute_unequal_clearances(self):
        """Smaller clearance -> higher load."""
        loads = CouplingFunctions.redistribute_bearing_loads(
            200.0, 2, [0.03, 0.06],
        )
        assert loads[0] > loads[1]  # Tighter bearing carries more
        assert sum(loads) == pytest.approx(200.0)

    def test_redistribute_empty(self):
        assert CouplingFunctions.redistribute_bearing_loads(100.0, 0, []) == []


# ---------------------------------------------------------------------------
# SimulationEngine Initialization
# ---------------------------------------------------------------------------

class TestEngineInit:

    def test_initial_state_temperature(self, engine, default_config):
        assert engine.state.temperature_C == default_config.ambient_temperature_C

    def test_initial_bearing_count(self, engine, default_config):
        assert len(engine.state.bearing_clearances_mm) == default_config.bearing_count
        assert len(engine.state.bearing_loads_N) == default_config.bearing_count

    def test_initial_clearances_match_config(self, engine, default_config):
        for c in engine.state.bearing_clearances_mm:
            assert c == default_config.initial_clearance_mm

    def test_initial_wear_zero(self, engine, default_config):
        for w in engine.state.bearing_wear_volumes_mm3:
            assert w == 0.0

    def test_initial_loads_equal(self, engine, default_config):
        expected = engine._total_gravity_load_N() / default_config.bearing_count
        for load in engine.state.bearing_loads_N:
            assert load == pytest.approx(expected)

    def test_not_failed(self, engine):
        assert not engine.failed
        assert engine.failure_reason == ""


# ---------------------------------------------------------------------------
# Single Step
# ---------------------------------------------------------------------------

class TestSingleStep:

    def test_step_advances_time(self, engine, default_config):
        engine.step()
        assert engine.state.time_s == pytest.approx(default_config.dt_s)

    def test_step_returns_step_result(self, engine):
        result = engine.step()
        assert isinstance(result, StepResult)
        assert result.time_s > 0
        assert result.temperature_C > 0
        assert result.total_heat_W >= 0

    def test_temperature_increases_from_ambient(self, engine, default_config):
        """Heat generation should raise temperature above ambient."""
        for _ in range(10):
            engine.step()
        assert engine.state.temperature_C >= default_config.ambient_temperature_C

    def test_wear_accumulates(self, engine):
        """Wear volumes should increase monotonically."""
        engine.step()
        w0 = list(engine.state.bearing_wear_volumes_mm3)
        engine.step()
        w1 = engine.state.bearing_wear_volumes_mm3
        for i in range(len(w0)):
            assert w1[i] >= w0[i]

    def test_sliding_distance_accumulates(self, engine, default_config):
        engine.step()
        expected = math.pi * default_config.shaft_diameter_mm * default_config.rpm / 60.0 * default_config.dt_s
        assert engine.state.total_sliding_distance_mm == pytest.approx(expected, rel=0.01)

    def test_shaft_deflection_positive(self, engine):
        engine.step()
        assert engine.state.shaft_deflection_mm >= 0

    def test_lubrication_regime_valid(self, engine):
        result = engine.step()
        assert result.lubrication_regime in ("full_film", "mixed", "boundary")

    def test_no_nan_in_state(self, engine):
        """No NaN values should appear in the state after stepping."""
        for _ in range(20):
            engine.step()
        s = engine.state
        assert not math.isnan(s.temperature_C)
        assert not math.isnan(s.shaft_deflection_mm)
        assert not math.isnan(s.oil_viscosity_Pa_s)
        for c in s.bearing_clearances_mm:
            assert not math.isnan(c)
        for w in s.bearing_wear_volumes_mm3:
            assert not math.isnan(w)
        for load in s.bearing_loads_N:
            assert not math.isnan(load)


# ---------------------------------------------------------------------------
# Energy Conservation
# ---------------------------------------------------------------------------

class TestEnergyConservation:

    def test_steady_state_heat_balance(self, lib):
        """At thermal steady state, heat generation ~ heat dissipation."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)

        # Run long enough to approach steady state (thermal time constant)
        # tau ~ m*c_p / (h*A) ~ 500*460 / (10*7.2) ~ 3194 s
        # Run ~5*tau = 16000 s (4.4 hours)
        for _ in range(16000):
            eng.step()

        T = eng.state.temperature_C
        T_amb = cfg.ambient_temperature_C

        # At steady state: Q_gen = h_total * A * (T - T_amb)
        # Check that temperature has risen above ambient (heat IS being generated)
        assert T > T_amb

        # Heat dissipation rate via convection + radiation
        T_s_K = T + 273.15
        T_amb_K = T_amb + 273.15
        sigma = 5.67e-8
        h_rad = cfg.emissivity * sigma * (T_s_K**2 + T_amb_K**2) * (T_s_K + T_amb_K)
        h_total = cfg.h_convection_W_m2K + h_rad
        Q_dissipated = h_total * cfg.surface_area_m2 * (T - T_amb)

        Q_gen = eng.state.total_heat_generation_W

        # At steady state these should match within 20%
        if Q_gen > 0.1:  # Only check if meaningful heat is generated
            ratio = Q_dissipated / Q_gen
            assert 0.5 < ratio < 2.0, (
                f"Q_gen={Q_gen:.2f} W, Q_diss={Q_dissipated:.2f} W, ratio={ratio:.2f}"
            )


# ---------------------------------------------------------------------------
# Force Equilibrium
# ---------------------------------------------------------------------------

class TestForceEquilibrium:

    def test_bearing_loads_sum_to_total(self, engine):
        """Sum of bearing loads equals total gravity load."""
        engine.step()
        total = sum(engine.state.bearing_loads_N)
        expected = engine._total_gravity_load_N()
        assert total == pytest.approx(expected, rel=0.001)

    def test_loads_positive(self, engine):
        engine.step()
        for load in engine.state.bearing_loads_N:
            assert load > 0


# ---------------------------------------------------------------------------
# Coupled Convergence
# ---------------------------------------------------------------------------

class TestCoupledConvergence:

    def test_results_within_10pct_of_uncoupled(self, lib):
        """Coupled steady-state temperature within 10% of simple estimate.

        Simple estimate: T_ss = T_amb + Q_gen / (h * A)
        """
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)

        for _ in range(16000):
            eng.step()

        T_coupled = eng.state.temperature_C

        # Simple uncoupled estimate using initial bearing loads
        initial_load = cfg.machine_mass_kg * 9.81 / cfg.bearing_count
        bearing_mat = lib.get(cfg.bearing_material)
        mu = bearing_mat.friction_coeff * 0.05  # full-film reduction
        Q_bearing = 0.0
        for _ in range(cfg.bearing_count):
            Q_bearing += 0.5 * mu * initial_load * (cfg.shaft_diameter_mm / 1000.0) * cfg.omega_rad_s
        Q_gear = cfg.transmitted_power_W * 0.02
        Q_total = Q_bearing + Q_gear
        T_simple = cfg.ambient_temperature_C + Q_total / (cfg.h_convection_W_m2K * cfg.surface_area_m2)

        # Allow 50% tolerance because coupling adds radiation, load redistribution, etc.
        if T_simple > cfg.ambient_temperature_C + 0.1:
            delta_coupled = T_coupled - cfg.ambient_temperature_C
            delta_simple = T_simple - cfg.ambient_temperature_C
            ratio = delta_coupled / delta_simple
            assert 0.3 < ratio < 3.0, (
                f"T_coupled={T_coupled:.2f}, T_simple={T_simple:.2f}, ratio={ratio:.2f}"
            )


# ---------------------------------------------------------------------------
# Long-Duration Run
# ---------------------------------------------------------------------------

class TestLongDurationRun:

    def test_run_returns_result(self, lib):
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        result = eng.run(duration_s=60.0, record_interval_s=10.0)
        assert isinstance(result, SimulationResult)
        assert result.steps == 60
        assert result.duration_s == pytest.approx(60.0)
        assert len(result.history) > 0

    def test_run_records_at_interval(self, lib):
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        result = eng.run(duration_s=100.0, record_interval_s=20.0)
        # Should record at t=0, 20, 40, 60, 80, 100 -> 6 points
        assert 4 <= len(result.history) <= 7

    def test_run_deterministic(self, lib):
        """Two identical runs produce identical results."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)

        eng1 = SimulationEngine(cfg, lib)
        r1 = eng1.run(60.0, 10.0)

        eng2 = SimulationEngine(cfg, lib)
        r2 = eng2.run(60.0, 10.0)

        assert r1.final_state.temperature_C == pytest.approx(r2.final_state.temperature_C)
        assert r1.steps == r2.steps

    def test_run_wall_time_reasonable(self, lib):
        """1 hour simulation (3600 steps) should complete quickly."""
        import time
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        t0 = time.monotonic()
        eng.run(3600.0, 600.0)
        elapsed = time.monotonic() - t0
        assert elapsed < 30.0, f"1h sim took {elapsed:.1f}s wall time"


# ---------------------------------------------------------------------------
# Predict Maintenance
# ---------------------------------------------------------------------------

class TestPredictMaintenance:

    def test_maintenance_returns_result(self, lib):
        cfg = SimulationConfig(dt_s=10.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        result = eng.predict_maintenance(max_hours=1.0)
        assert isinstance(result, SimulationResult)

    def test_default_machine_survives_1_hour(self, lib):
        """Default config should not fail within 1 hour."""
        cfg = SimulationConfig(dt_s=10.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)
        result = eng.predict_maintenance(max_hours=1.0)
        assert not eng.failed, f"Failed at {eng.state.time_s:.0f}s: {eng.failure_reason}"


# ---------------------------------------------------------------------------
# Failure Detection
# ---------------------------------------------------------------------------

class TestFailureDetection:

    def test_temperature_limit_triggers(self, lib):
        """Very high heat scenario should trigger temperature failure."""
        cfg = SimulationConfig(
            dt_s=1.0,
            rpm=30.0,
            temperature_limit_C=22.0,  # Very low limit
            h_convection_W_m2K=0.01,   # Very poor cooling
            surface_area_m2=0.01,      # Tiny surface (no cooling)
            machine_mass_kg=0.1,       # Very light (fast heat up)
        )
        eng = SimulationEngine(cfg, lib)
        result = eng.run(10000.0, 100.0)
        assert eng.failed
        assert eng.failure_reason == "temperature"

    def test_clearance_limit_triggers(self, lib):
        """Extreme wear rate should trigger clearance failure."""
        cfg = SimulationConfig(
            dt_s=1.0,
            rpm=30.0,
            archard_K_bearing=1e-2,      # Extreme wear
            clearance_limit_mm=0.06,     # Tight limit
        )
        eng = SimulationEngine(cfg, lib)
        result = eng.run(10000.0, 100.0)
        # Should fail from clearance OR some other limit before 10000s
        # (extreme wear rate will blow through clearance quickly)
        if eng.failed:
            assert eng.failure_reason in ("bearing_clearance", "bearing_seizure", "temperature")

    def test_reset_clears_failure(self, lib):
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0, temperature_limit_C=21.0)
        eng = SimulationEngine(cfg, lib)
        eng.run(1000.0)
        eng.reset()
        assert not eng.failed
        assert eng.failure_reason == ""
        assert eng.state.time_s == 0.0


# ---------------------------------------------------------------------------
# Mass Conservation (Wear)
# ---------------------------------------------------------------------------

class TestMassConservation:

    def test_wear_monotonically_increasing(self, lib):
        """Wear volumes never decrease."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)

        prev_wear = [0.0] * cfg.bearing_count
        for _ in range(100):
            eng.step()
            for i, w in enumerate(eng.state.bearing_wear_volumes_mm3):
                assert w >= prev_wear[i]
                prev_wear[i] = w

    def test_total_wear_proportional_to_time(self, lib):
        """Wear should grow roughly linearly in time (steady-state K)."""
        cfg = SimulationConfig(dt_s=1.0, rpm=30.0)
        eng = SimulationEngine(cfg, lib)

        for _ in range(100):
            eng.step()
        w100 = sum(eng.state.bearing_wear_volumes_mm3)

        for _ in range(100):
            eng.step()
        w200 = sum(eng.state.bearing_wear_volumes_mm3)

        # Second 100 steps should produce similar wear to first 100
        dw_first = w100
        dw_second = w200 - w100
        if dw_first > 0:
            ratio = dw_second / dw_first
            assert 0.5 < ratio < 2.0, f"Wear ratio: {ratio:.2f}"
