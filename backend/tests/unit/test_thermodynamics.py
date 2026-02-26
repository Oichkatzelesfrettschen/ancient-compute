"""Tests for Thermodynamic Model.

Validates:
- Heat generation is positive for all sources
- Thermal expansion: brass > steel (higher CTE)
- Steady-state temperature rise is in a reasonable range
- Thermal time constant is positive
- Dimensional consistency
"""

import math
import pytest

from backend.src.emulator.thermodynamics import (
    FrictionHeatModel,
    ThermalExpansionModel,
    RadiationHeatModel,
    TransientThermalSolver,
    ThermalClearanceFeedback,
    OperatingEnvelope,
    compute_engine_thermal_model,
    compute_steady_state_rise_C,
    compute_thermal_time_constant_s,
)
from backend.src.emulator.materials import MaterialLibrary


@pytest.fixture
def lib():
    return MaterialLibrary()


@pytest.fixture
def envelope():
    return compute_engine_thermal_model()


# -- Friction Heat --

class TestFrictionHeat:
    def test_bearing_heat_positive(self):
        q = FrictionHeatModel.bearing_heat_W(0.10, 1000.0, 50.0, math.pi)
        assert q > 0

    def test_bearing_heat_proportional_to_load(self):
        q1 = FrictionHeatModel.bearing_heat_W(0.10, 500.0, 50.0, math.pi)
        q2 = FrictionHeatModel.bearing_heat_W(0.10, 1000.0, 50.0, math.pi)
        assert q2 == pytest.approx(2.0 * q1)

    def test_gear_mesh_heat_positive(self):
        q = FrictionHeatModel.gear_mesh_heat_W(100.0, 0.97)
        assert q > 0

    def test_gear_mesh_zero_loss_at_unity_efficiency(self):
        q = FrictionHeatModel.gear_mesh_heat_W(100.0, 1.0)
        assert q == pytest.approx(0.0)

    def test_cam_friction_heat_positive(self):
        q = FrictionHeatModel.cam_friction_heat_W(0.12, 200.0, 0.05)
        assert q > 0


# -- Thermal Expansion --

class TestThermalExpansion:
    def test_linear_expansion_positive(self):
        delta = ThermalExpansionModel.linear_expansion_mm(20.5e-6, 100.0, 20.0)
        assert delta > 0

    def test_brass_expands_more_than_steel(self, lib):
        brass = lib.get("brass")
        steel = lib.get("steel")
        length = 100.0
        dt = 20.0
        d_brass = ThermalExpansionModel.linear_expansion_mm(
            brass.thermal_expansion_coeff_per_K, length, dt
        )
        d_steel = ThermalExpansionModel.linear_expansion_mm(
            steel.thermal_expansion_coeff_per_K, length, dt
        )
        assert d_brass > d_steel, "Brass CTE > steel CTE, so brass must expand more"

    def test_dissimilar_clearance_change(self, lib):
        brass = lib.get("brass")
        steel = lib.get("steel")
        # Brass bushing on steel shaft, 50mm bore, 20K rise
        delta = ThermalExpansionModel.dissimilar_metal_clearance_change_mm(
            brass.thermal_expansion_coeff_per_K, 50.0,
            steel.thermal_expansion_coeff_per_K, 50.0,
            20.0,
        )
        # Brass expands more -> positive delta -> clearance increases
        assert delta > 0

    def test_gear_backlash_change(self, lib):
        brass = lib.get("brass")
        steel = lib.get("steel")
        delta = ThermalExpansionModel.gear_backlash_change_mm(
            brass.thermal_expansion_coeff_per_K,
            steel.thermal_expansion_coeff_per_K,
            100.0,  # center distance
            20.0,   # delta T
        )
        # Brass gears on steel shafts: brass CTE > steel CTE -> positive
        assert delta > 0

    def test_bearing_clearance_change(self, lib):
        pb = lib.get("phosphor_bronze")
        steel = lib.get("steel")
        delta = ThermalExpansionModel.bearing_clearance_change_mm(
            pb.thermal_expansion_coeff_per_K,
            steel.thermal_expansion_coeff_per_K,
            50.0,  # bore diameter
            20.0,  # delta T
        )
        # PB CTE > steel CTE -> clearance increases
        assert delta > 0

    def test_zero_delta_t_gives_zero_expansion(self):
        delta = ThermalExpansionModel.linear_expansion_mm(20.5e-6, 100.0, 0.0)
        assert delta == 0.0


# -- Thermal Time Constant --

class TestThermalTimeConstant:
    def test_positive(self):
        tau = compute_thermal_time_constant_s(500.0, 490.0, 7.0, 10.0)
        assert tau > 0

    def test_larger_mass_longer_constant(self):
        tau1 = compute_thermal_time_constant_s(250.0, 490.0, 7.0, 10.0)
        tau2 = compute_thermal_time_constant_s(500.0, 490.0, 7.0, 10.0)
        assert tau2 > tau1

    def test_zero_area_gives_inf(self):
        tau = compute_thermal_time_constant_s(500.0, 490.0, 0.0, 10.0)
        assert tau == float("inf")

    def test_steady_state_rise_positive(self):
        dt = compute_steady_state_rise_C(50.0, 7.0, 10.0)
        assert dt > 0

    def test_zero_heat_zero_rise(self):
        dt = compute_steady_state_rise_C(0.0, 7.0, 10.0)
        assert dt == 0.0


# -- Operating Envelope --

class TestOperatingEnvelope:
    def test_loads_from_schema(self, envelope):
        assert envelope is not None

    def test_total_heat_positive(self, envelope):
        assert envelope.total_heat_generation_W > 0

    def test_total_heat_reasonable(self, envelope):
        # Expected ~10-50 W total from bearings + gears + cams
        assert 1.0 < envelope.total_heat_generation_W < 500.0

    def test_has_bearing_sources(self, envelope):
        bearing_sources = [s for s in envelope.heat_sources if s.source_type == "bearing"]
        assert len(bearing_sources) == 4

    def test_has_gear_sources(self, envelope):
        gear_sources = [s for s in envelope.heat_sources if s.source_type == "gear"]
        assert len(gear_sources) == 2

    def test_time_constant_positive(self, envelope):
        assert envelope.thermal_time_constant_s > 0

    def test_time_constant_reasonable(self, envelope):
        # 500 kg machine: expect ~30-120 min warm-up
        tau_min = envelope.thermal_time_constant_min
        assert 10 < tau_min < 1000, f"Time constant {tau_min:.0f} min out of range"

    def test_steady_state_rise_small(self, envelope):
        # With ~10-50 W in a large machine, expect < 5 C rise
        assert envelope.steady_state_rise_C < 20.0

    def test_operating_range(self, envelope):
        assert envelope.operating_T_min_C >= 0
        assert envelope.operating_T_max_C <= 80
        assert envelope.operating_T_max_C > envelope.operating_T_min_C

    def test_all_sources_named(self, envelope):
        for s in envelope.heat_sources:
            assert s.name
            assert s.source_type in ("bearing", "gear", "cam")


# ---------------------------------------------------------------------------
# Phase II: Regression - Monte Carlo gear backlash unit error
# ---------------------------------------------------------------------------

class TestGearBacklashRegression:
    """Regression: gear backlash delta must be in mm, not um.

    Historical bug: extra *1000 factor made backlash appear 1000x too large.
    Formula: delta_backlash = (alpha_gear - alpha_shaft) * c * dT [mm].
    """

    def test_backlash_units_are_mm(self, lib):
        """Backlash change for 50mm center distance, 30K rise should be < 0.1 mm."""
        brass = lib.get("brass")
        steel = lib.get("steel")
        delta = ThermalExpansionModel.gear_backlash_change_mm(
            alpha_gear=brass.thermal_expansion_coeff_per_K,
            alpha_shaft=steel.thermal_expansion_coeff_per_K,
            center_distance_mm=50.0,
            delta_T_K=30.0,
        )
        # Expected: ~(19e-6 - 12e-6) * 50 * 30 = ~0.0105 mm
        assert 0.001 < abs(delta) < 0.1, (
            f"Backlash change {delta:.6f} mm out of expected range"
        )

    def test_backlash_positive_when_gear_expands_more(self, lib):
        """When gear CTE > shaft CTE, backlash decreases (gap closes)."""
        brass = lib.get("brass")
        steel = lib.get("steel")
        delta = ThermalExpansionModel.gear_backlash_change_mm(
            alpha_gear=brass.thermal_expansion_coeff_per_K,
            alpha_shaft=steel.thermal_expansion_coeff_per_K,
            center_distance_mm=50.0,
            delta_T_K=30.0,
        )
        # Brass CTE > steel CTE, so delta should be positive
        assert delta > 0


# ---------------------------------------------------------------------------
# Phase VI: Radiation Heat Loss
# ---------------------------------------------------------------------------

class TestRadiationHeatModel:
    def test_radiation_positive_when_hot(self):
        Q = RadiationHeatModel.radiation_heat_W(0.9, 1.0, 313.0, 293.0)
        assert Q > 0

    def test_radiation_zero_at_ambient(self):
        Q = RadiationHeatModel.radiation_heat_W(0.9, 1.0, 293.0, 293.0)
        assert Q == pytest.approx(0.0)

    def test_radiation_T4_dependence(self):
        """Doubling T_s should produce >> 2x heat (T^4 law)."""
        Q1 = RadiationHeatModel.radiation_heat_W(0.9, 1.0, 300.0, 293.0)
        Q2 = RadiationHeatModel.radiation_heat_W(0.9, 1.0, 350.0, 293.0)
        assert Q2 > 5 * Q1  # Much more than proportional

    def test_linearized_h_positive(self):
        h = RadiationHeatModel.linearized_h_rad_W_m2K(0.9, 313.0, 293.0)
        assert h > 0

    def test_total_heat_lower_with_radiation(self):
        """Adding radiation cooling lowers steady-state temperature."""
        Q_in = 20.0
        A = 7.0
        h_conv = 10.0
        T_ss_conv = 20.0 + Q_in / (h_conv * A)
        h_rad = RadiationHeatModel.linearized_h_rad_W_m2K(0.9, 313.0, 293.0)
        T_ss_total = 20.0 + Q_in / ((h_conv + h_rad) * A)
        assert T_ss_total < T_ss_conv


# ---------------------------------------------------------------------------
# Phase VI: Transient Thermal Solver
# ---------------------------------------------------------------------------

class TestTransientThermalSolver:
    def test_euler_approaches_steady_state(self):
        Q_in = 20.0
        h = 10.0
        A = 7.0
        m = 500.0
        cp = 460.0
        T_amb = 20.0
        T_ss = TransientThermalSolver.steady_state_T_C(Q_in, h, A, T_amb)
        curve = TransientThermalSolver.warmup_curve(
            Q_in, h, A, m, cp, T_amb,
            duration_s=100000.0, dt_s=100.0, method="euler",
        )
        T_final = curve[-1][1]
        assert T_final == pytest.approx(T_ss, rel=0.05)

    def test_crank_nicolson_approaches_steady_state(self):
        Q_in = 20.0
        h = 10.0
        A = 7.0
        m = 500.0
        cp = 460.0
        T_amb = 20.0
        T_ss = TransientThermalSolver.steady_state_T_C(Q_in, h, A, T_amb)
        curve = TransientThermalSolver.warmup_curve(
            Q_in, h, A, m, cp, T_amb,
            duration_s=100000.0, dt_s=100.0, method="crank_nicolson",
        )
        T_final = curve[-1][1]
        assert T_final == pytest.approx(T_ss, rel=0.05)

    def test_euler_cn_converge_to_same_steady(self):
        Q_in = 20.0
        h = 10.0
        A = 7.0
        m = 500.0
        cp = 460.0
        T_amb = 20.0
        curve_e = TransientThermalSolver.warmup_curve(
            Q_in, h, A, m, cp, T_amb,
            duration_s=100000.0, dt_s=100.0, method="euler",
        )
        curve_cn = TransientThermalSolver.warmup_curve(
            Q_in, h, A, m, cp, T_amb,
            duration_s=100000.0, dt_s=100.0, method="crank_nicolson",
        )
        assert curve_e[-1][1] == pytest.approx(curve_cn[-1][1], rel=0.05)

    def test_warmup_63pct_at_tau(self):
        """At t=tau, temperature should reach ~63.2% of steady-state rise."""
        Q_in = 20.0
        h = 10.0
        A = 7.0
        m = 500.0
        cp = 460.0
        T_amb = 20.0
        tau = TransientThermalSolver.time_constant_s(m, cp, h, A)
        T_ss = TransientThermalSolver.steady_state_T_C(Q_in, h, A, T_amb)
        delta_T_ss = T_ss - T_amb
        # Run to tau
        curve = TransientThermalSolver.warmup_curve(
            Q_in, h, A, m, cp, T_amb,
            duration_s=tau, dt_s=tau / 100.0,
        )
        T_at_tau = curve[-1][1]
        fraction = (T_at_tau - T_amb) / delta_T_ss
        assert fraction == pytest.approx(0.632, abs=0.05)


# ---------------------------------------------------------------------------
# Phase VI: Thermal-Clearance Feedback
# ---------------------------------------------------------------------------

class TestThermalClearanceFeedback:
    def test_thermal_clearance_change(self, lib):
        pb = lib.get("phosphor_bronze")
        steel = lib.get("steel")
        delta = ThermalClearanceFeedback.thermal_clearance_mm(
            pb.thermal_expansion_coeff_per_K,
            steel.thermal_expansion_coeff_per_K,
            50.0, 40.0, 20.0,
        )
        # Bronze CTE > steel CTE, clearance should increase
        assert delta > 0

    def test_combined_clearance(self):
        c = ThermalClearanceFeedback.combined_clearance_mm(0.05, 0.002, 0.001)
        assert c == pytest.approx(0.053)

    def test_no_seizure_in_normal_range(self, lib):
        """No seizure in 10-40 C operating range."""
        pb = lib.get("phosphor_bronze")
        steel = lib.get("steel")
        for T in [10.0, 20.0, 30.0, 40.0]:
            delta = ThermalClearanceFeedback.thermal_clearance_mm(
                pb.thermal_expansion_coeff_per_K,
                steel.thermal_expansion_coeff_per_K,
                50.0, T, 20.0,
            )
            c = ThermalClearanceFeedback.combined_clearance_mm(0.05, delta, 0.0)
            assert not ThermalClearanceFeedback.is_seized(c), (
                f"Seizure at T={T} C, clearance={c:.4f} mm"
            )
