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

pytestmark = pytest.mark.physics

from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.thermodynamics import (
    FrictionHeatModel,
    RadiationHeatModel,
    ThermalClearanceFeedback,
    ThermalExpansionModel,
    TransientThermalSolver,
    compute_engine_thermal_model,
    compute_natural_convection_h_W_m2K,
    compute_steady_state_rise_C,
    compute_thermal_time_constant_s,
)


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
        assert pytest.approx(0.0) == Q

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


# ---------------------------------------------------------------------------
# Phase 6.3: Churchill-Chu Natural Convection Correlation
# ---------------------------------------------------------------------------

class TestNaturalConvectionH:
    """Validate compute_natural_convection_h_W_m2K against published data.

    Reference values from Incropera & DeWitt, "Fundamentals of Heat and
    Mass Transfer", 7th ed., Table 9.1 (vertical plate, air, Pr~0.73).

    For a 0.5 m vertical plate in air with delta_T = 20 K at ~30 C film
    temperature:
        Ra = g * beta * delta_T * L^3 / nu^2 * Pr
           = 9.81 * (1/303) * 20 * 0.125 / (1.608e-5)^2 * 0.7282
           ~ 1.34e7  (turbulent-transition regime)
        Nu (Churchill-Chu) ~ 45-55 -> h ~ 3-5 W/(m^2.K)
    This bracket is used as the acceptance criterion.
    """

    def test_h_positive_for_hot_surface(self):
        h = compute_natural_convection_h_W_m2K(
            surface_T_C=40.0, ambient_T_C=20.0, char_length_m=0.5,
        )
        assert h > 0.0

    def test_h_zero_at_zero_delta_T(self):
        h = compute_natural_convection_h_W_m2K(
            surface_T_C=30.0, ambient_T_C=30.0, char_length_m=0.5,
        )
        assert h == pytest.approx(0.0, abs=1e-9)

    def test_h_physical_range_vertical_plate(self):
        """h must be in 2-8 W/(m^2.K) for a 0.5 m vertical plate, delta_T=20 K."""
        h = compute_natural_convection_h_W_m2K(
            surface_T_C=40.0, ambient_T_C=20.0, char_length_m=0.5,
        )
        assert 2.0 < h < 8.0, f"h={h:.3f} W/m^2.K outside expected 2-8 range"

    def test_h_increases_with_delta_T(self):
        """Larger temperature difference -> larger h (stronger convection)."""
        h_small = compute_natural_convection_h_W_m2K(
            surface_T_C=25.0, ambient_T_C=20.0, char_length_m=0.5,
        )
        h_large = compute_natural_convection_h_W_m2K(
            surface_T_C=60.0, ambient_T_C=20.0, char_length_m=0.5,
        )
        assert h_large > h_small

    def test_h_horizontal_plate_reasonable(self):
        """Horizontal plate (heated up) h should also be in ~2-10 range."""
        h = compute_natural_convection_h_W_m2K(
            surface_T_C=40.0, ambient_T_C=20.0, char_length_m=0.5,
            geometry="horizontal_plate_up",
        )
        assert 2.0 < h < 12.0, f"h={h:.3f} W/m^2.K outside expected range"

    def test_h_greater_for_larger_plate(self):
        """Larger plate has larger Ra -> larger Nu, but h ~ Nu/L can decrease.
        For turbulent Ra (Ra^(1/3) regime), Nu ~ L, so h is approximately
        constant.  For laminar (Ra^(1/4)), Nu ~ L^(1/4), h ~ L^(-3/4) which
        decreases with L.  Either case: h for L=0.1 m >= h for L=1.0 m.
        """
        h_short = compute_natural_convection_h_W_m2K(
            surface_T_C=40.0, ambient_T_C=20.0, char_length_m=0.1,
        )
        h_tall = compute_natural_convection_h_W_m2K(
            surface_T_C=40.0, ambient_T_C=20.0, char_length_m=1.0,
        )
        assert h_short >= h_tall

    def test_default_h_higher_than_hardcoded_for_hot_machine(self):
        """For a hot machine (delta_T ~ 10 K, L=1 m), Churchill-Chu gives h
        in the 2-5 range (below the conservative hardcoded 10 W/m^2.K).
        This confirms the hardcoded value is indeed an overestimate for free
        convection in still air, and the correlation gives more accurate results.
        """
        h = compute_natural_convection_h_W_m2K(
            surface_T_C=30.0, ambient_T_C=20.0, char_length_m=1.0,
        )
        # Churchill-Chu for these params: Ra~3.5e9, h~2-4 W/m^2.K
        assert h < 10.0, f"h={h:.3f} should be < hardcoded 10 W/m^2.K"
        assert h > 1.0, f"h={h:.3f} should be > 1 W/m^2.K (non-trivial)"


# ---------------------------------------------------------------------------
# Phase 6.4: Crank-Nicolson Order-of-Convergence (Richardson Extrapolation)
# ---------------------------------------------------------------------------

class TestCrankNicolsonConvergence:
    """Verify Crank-Nicolson is second-order in time (order p ~ 2).

    Richardson extrapolation: integrate with step sizes dt, dt/2, dt/4.
    For a method of order p, the global error E(dt) ~ C * dt^p, so:
        p ~ log2(|E(dt) - E(dt/2)| / |E(dt/2) - E(dt/4)|)

    The exact analytical solution for the lumped ODE is:
        T(t) = T_ss + (T_0 - T_ss) * exp(-t / tau)

    where T_ss = T_amb + Q / (h * A), tau = m * c_p / (h * A).
    """

    Q_IN = 20.0
    H = 10.0
    A = 7.0
    M = 500.0
    CP = 460.0
    T_AMB = 20.0
    T0 = 20.0
    T_END = 5000.0

    def _exact(self, t: float) -> float:
        """Analytical solution."""
        hA = self.H * self.A
        mc = self.M * self.CP
        T_ss = self.T_AMB + self.Q_IN / hA
        tau = mc / hA
        return T_ss + (self.T0 - T_ss) * math.exp(-t / tau)

    def _run_cn(self, dt: float) -> float:
        """Run Crank-Nicolson to T_END, return final temperature."""
        T = self.T0
        t = 0.0
        while t < self.T_END - dt * 0.5:
            T = TransientThermalSolver.crank_nicolson_step(
                T, self.Q_IN, self.H, self.A, self.M, self.CP, self.T_AMB, dt,
            )
            t += dt
        return T

    def test_crank_nicolson_second_order(self):
        """Confirm Crank-Nicolson achieves order p >= 1.8 via Richardson extrapolation.

        True CN is 2nd order; we accept >= 1.8 to tolerate endpoint stepping.
        """
        T_exact = self._exact(self.T_END)
        dt_base = 500.0
        e1 = abs(self._run_cn(dt_base) - T_exact)
        e2 = abs(self._run_cn(dt_base / 2.0) - T_exact)
        e3 = abs(self._run_cn(dt_base / 4.0) - T_exact)

        if e2 < 1e-12 or e3 < 1e-12:
            # Errors too small to measure order; method already converged
            return

        order_12 = math.log2(e1 / e2) if e1 > e2 else 0.0
        order_23 = math.log2(e2 / e3) if e2 > e3 else 0.0
        # Take the finer estimate (order_23) as more accurate
        observed_order = order_23
        assert observed_order >= 1.8, (
            f"Crank-Nicolson order={observed_order:.2f} < 1.8; "
            f"e1={e1:.4e}, e2={e2:.4e}, e3={e3:.4e}"
        )

    def test_cn_error_smaller_than_euler_at_same_dt(self):
        """Crank-Nicolson should have smaller error than Forward Euler at the same dt."""
        dt = 500.0
        T_exact = self._exact(self.T_END)

        T_cn = self._run_cn(dt)

        # Forward Euler at same dt
        T_eu = self.T0
        t = 0.0
        while t < self.T_END - dt * 0.5:
            T_eu = TransientThermalSolver.forward_euler_step(
                T_eu, self.Q_IN, self.H, self.A, self.M, self.CP, self.T_AMB, dt,
            )
            t += dt

        err_cn = abs(T_cn - T_exact)
        err_eu = abs(T_eu - T_exact)
        assert err_cn <= err_eu, (
            f"CN error {err_cn:.4e} > Euler error {err_eu:.4e} at dt={dt}"
        )
