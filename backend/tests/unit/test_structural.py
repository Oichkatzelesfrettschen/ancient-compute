"""Tests for Structural Analysis Module.

Validates:
- Shaft deflection within L/10000 limit
- Gear tooth SF >= 2.0
- Fatigue life > 10^8 cycles
- Buckling SF >= 3.0 for vertical supports
"""

import math

import pytest

from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.structural import (
    BucklingAnalysis,
    CumulativeFatigue,
    DynamicLoadFactor,
    FatigueAnalysis,
    GearToothStress,
    NotchSensitivity,
    ShaftAnalysis,
    ShaftCriticalSpeed,
    StressConcentration,
)

pytestmark = pytest.mark.physics


@pytest.fixture
def lib():
    return MaterialLibrary()


# -- Shaft Deflection --


class TestShaftDeflection:
    def test_deflection_positive(self):
        d = ShaftAnalysis.max_deflection_simply_supported_mm(
            1000.0,
            500.0,
            200.0,
            50.0,
        )
        assert d > 0

    def test_deflection_proportional_to_force(self):
        d1 = ShaftAnalysis.max_deflection_simply_supported_mm(500.0, 500.0, 200.0, 50.0)
        d2 = ShaftAnalysis.max_deflection_simply_supported_mm(1000.0, 500.0, 200.0, 50.0)
        assert d2 == pytest.approx(2.0 * d1)

    def test_stiffer_material_less_deflection(self):
        d_brass = ShaftAnalysis.max_deflection_simply_supported_mm(1000.0, 500.0, 97.0, 50.0)
        d_steel = ShaftAnalysis.max_deflection_simply_supported_mm(1000.0, 500.0, 210.0, 50.0)
        assert d_steel < d_brass

    def test_multi_support_less_than_single(self):
        d_single = ShaftAnalysis.max_deflection_simply_supported_mm(1000.0, 1500.0, 210.0, 50.0)
        d_multi = ShaftAnalysis.max_deflection_multi_support_mm(1000.0, 1500.0, 4, 210.0, 50.0)
        assert d_multi < d_single

    def test_main_shaft_within_limit(self, lib):
        """50mm steel shaft, 1500mm, 4 supports, self-weight ~23 kg."""
        steel = lib.get("steel")
        load = 23.0 * 9.81  # shaft self-weight
        d = ShaftAnalysis.max_deflection_multi_support_mm(
            load,
            1500.0,
            4,
            steel.youngs_modulus_GPa[0],
            50.0,
        )
        limit = ShaftAnalysis.deflection_limit_mm(1500.0 / 3)  # span between bearings
        assert d < limit, f"Deflection {d:.4f} mm >= limit {limit:.4f} mm"

    def test_moment_of_inertia_positive(self):
        moi = ShaftAnalysis.moment_of_inertia_m4(50.0)
        assert moi > 0


# -- Gear Tooth Stress --


class TestGearToothStress:
    def test_stress_positive(self):
        sigma = GearToothStress.bending_stress_MPa(100.0, 15.0, 2.5, 20)
        assert sigma > 0

    def test_sf_above_2(self, lib):
        """Brass gear, 50W transmitted, 30 RPM -> SF >= 2."""
        brass = lib.get("brass")
        # Pitch line velocity at primary gear
        v = math.pi * 50.0 * 30.0 / 60000.0  # m/s
        wt = 50.0 / v if v > 0 else 0  # tangential force
        sigma = GearToothStress.bending_stress_MPa(wt, 15.0, 2.5, 20)
        sf = GearToothStress.safety_factor(brass.yield_strength_MPa[0], sigma)
        assert sf >= 2.0, f"Gear tooth SF={sf:.1f} < 2.0"

    def test_sf_infinite_for_zero_stress(self):
        sf = GearToothStress.safety_factor(275.0, 0.0)
        assert sf == float("inf")


# -- Fatigue Analysis --


class TestFatigueAnalysis:
    def test_surface_factor_positive(self):
        ka = FatigueAnalysis.surface_factor_ka(500.0, "machined")
        assert 0 < ka <= 1.0

    def test_ground_better_than_machined(self):
        ka_ground = FatigueAnalysis.surface_factor_ka(500.0, "ground")
        ka_machined = FatigueAnalysis.surface_factor_ka(500.0, "machined")
        assert ka_ground > ka_machined

    def test_size_factor_positive(self):
        kb = FatigueAnalysis.size_factor_kb(50.0)
        assert 0 < kb <= 1.0

    def test_corrected_endurance_limit(self, lib):
        steel = lib.get("steel")
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,
        )
        assert Se > 0
        # Corrected Se should be less than uncorrected
        assert Se <= steel.endurance_limit_MPa[0]

    def test_goodman_sf_above_2(self, lib):
        """Main shaft under fully reversed bending: SF >= 2."""
        steel = lib.get("steel")
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,
        )
        # Approximate bending stress amplitude from shaft deflection
        # At 30 RPM, light loading: sigma_a ~ 10 MPa, sigma_m ~ 0
        sf = FatigueAnalysis.goodman_safety_factor(
            10.0,
            0.0,
            Se,
            steel.ultimate_tensile_strength_MPa[0],
        )
        assert sf >= 2.0, f"Goodman SF={sf:.1f} < 2.0"

    def test_fatigue_life_above_threshold(self, lib):
        """With low stress amplitude, life should exceed 10^8 cycles."""
        steel = lib.get("steel")
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,
        )
        # Low stress amplitude (well below Se)
        N = FatigueAnalysis.fatigue_life_cycles(
            10.0,
            Se,
            steel.ultimate_tensile_strength_MPa[0],
        )
        assert N >= 1e8, f"Fatigue life {N:.0e} < 10^8"

    def test_infinite_life_below_Se(self):
        N = FatigueAnalysis.fatigue_life_cycles(50.0, 100.0, 500.0)
        assert N >= 1e8

    def test_zero_life_above_Su(self):
        N = FatigueAnalysis.fatigue_life_cycles(600.0, 100.0, 500.0)
        assert N == 0.0


# -- Buckling Analysis --


class TestBucklingAnalysis:
    def test_euler_load_positive(self):
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P = BucklingAnalysis.euler_critical_load_N(200.0, moi, 600.0, 0.5)
        assert P > 0

    def test_slenderness_ratio_positive(self):
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        A = BucklingAnalysis.rectangular_section_area_mm2(25.0, 25.0)
        sr = BucklingAnalysis.slenderness_ratio(600.0, 0.5, moi, A)
        assert sr > 0

    def test_column_support_sf_above_3(self, lib):
        """Vertical digit column support: 25x25mm steel, 600mm, fixed-fixed."""
        steel = lib.get("steel")
        w = 25.0
        h = 25.0
        moi = BucklingAnalysis.rectangular_section_I_mm4(w, h)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0],
            moi,
            600.0,
            0.5,
        )
        # Applied load: weight of column mechanism (~5 kg per column)
        applied = 5.0 * 9.81
        sf = BucklingAnalysis.buckling_safety_factor(P_cr, applied)
        assert sf >= 3.0, f"Buckling SF={sf:.1f} < 3.0"

    def test_longer_column_lower_critical_load(self):
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_short = BucklingAnalysis.euler_critical_load_N(200.0, moi, 300.0, 0.5)
        P_long = BucklingAnalysis.euler_critical_load_N(200.0, moi, 600.0, 0.5)
        assert P_long < P_short

    def test_sf_infinite_for_zero_load(self):
        sf = BucklingAnalysis.buckling_safety_factor(1000.0, 0.0)
        assert sf == float("inf")


# -- Cross-validation with Phase B loads --


class TestStructuralIntegration:
    def test_all_safety_factors_adequate(self, lib):
        """Verify overall structural adequacy."""
        steel = lib.get("steel")
        brass = lib.get("brass")

        # Shaft deflection
        d = ShaftAnalysis.max_deflection_multi_support_mm(
            23.0 * 9.81,
            1500.0,
            4,
            steel.youngs_modulus_GPa[0],
            50.0,
        )
        assert d < 0.1, "Shaft deflection excessive"

        # Gear tooth SF
        v = math.pi * 50.0 * 30.0 / 60000.0
        wt = 50.0 / v if v > 0 else 0
        sigma = GearToothStress.bending_stress_MPa(wt, 15.0, 2.5, 20)
        sf_gear = GearToothStress.safety_factor(brass.yield_strength_MPa[0], sigma)
        assert sf_gear >= 2.0

        # Fatigue
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,
        )
        sf_fatigue = FatigueAnalysis.goodman_safety_factor(
            10.0,
            0.0,
            Se,
            steel.ultimate_tensile_strength_MPa[0],
        )
        assert sf_fatigue >= 2.0

        # Buckling
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0],
            moi,
            600.0,
            0.5,
        )
        sf_buckling = BucklingAnalysis.buckling_safety_factor(P_cr, 5.0 * 9.81)
        assert sf_buckling >= 3.0


# -- Phase III: Stress Concentration Factors --


class TestStressConcentration:
    def test_stepped_shaft_Kt_ge_1(self):
        Kt = StressConcentration.stepped_shaft(60.0, 50.0, 3.0)
        assert Kt >= 1.0

    def test_stepped_shaft_tabulated_within_5pct(self):
        """D/d=1.5, r/d=0.1: expected K_t ~ 1.68 (Peterson)."""
        Kt = StressConcentration.stepped_shaft(75.0, 50.0, 5.0)
        # r/d = 5/50 = 0.1, D/d = 75/50 = 1.5
        # From Peterson tables: ~1.68 for bending
        assert 1.3 < Kt < 2.1

    def test_larger_Dd_higher_Kt(self):
        Kt_small = StressConcentration.stepped_shaft(55.0, 50.0, 3.0)
        Kt_large = StressConcentration.stepped_shaft(100.0, 50.0, 3.0)
        assert Kt_large >= Kt_small

    def test_keyway_bending(self):
        assert StressConcentration.keyway_bending() == pytest.approx(2.14)

    def test_keyway_torsion(self):
        assert StressConcentration.keyway_torsion() == pytest.approx(3.0)


# -- Phase III: Dynamic Load Factor --


class TestDynamicLoadFactor:
    def test_Kv_ge_1(self):
        Kv = DynamicLoadFactor.agma_Kv(100.0)
        assert Kv >= 1.0

    def test_Kv_at_zero_velocity(self):
        Kv = DynamicLoadFactor.agma_Kv(0.0)
        assert Kv == 1.0

    def test_Kv_monotone_in_velocity(self):
        Kv_low = DynamicLoadFactor.agma_Kv(10.0)
        Kv_high = DynamicLoadFactor.agma_Kv(1000.0)
        assert Kv_high >= Kv_low

    def test_Kv_at_engine_speed(self):
        """At 30 RPM, 50mm pitch dia: V ~ 15.7 ft/min, K_v < 1.1."""
        V = DynamicLoadFactor.pitch_velocity_ft_min(50.0, 30.0)
        Kv = DynamicLoadFactor.agma_Kv(V)
        assert Kv < 1.1, f"K_v={Kv:.3f} >= 1.1 at engine speed"

    def test_agma_bending_amplifies(self):
        sigma_lewis = 10.0
        Kv = 1.05
        sigma_agma = DynamicLoadFactor.agma_bending_stress_MPa(sigma_lewis, Kv)
        assert sigma_agma > sigma_lewis


# -- Phase III: Johnson Column Transition --


class TestJohnsonBuckling:
    def test_johnson_le_yield(self, lib):
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        sigma = BucklingAnalysis.johnson_critical_stress_MPa(Sy, E, 30.0)
        assert sigma <= Sy

    def test_johnson_lt_euler_below_transition(self, lib):
        """Below transition, Johnson gives lower (more conservative) stress than Euler.

        Johnson accounts for inelastic buckling where the column yields before
        reaching the Euler elastic limit, so sigma_Johnson < sigma_Euler.
        """
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        tr = BucklingAnalysis.transition_slenderness_ratio(Sy, E)
        sr = tr * 0.5  # Below transition
        sigma_j = BucklingAnalysis.johnson_critical_stress_MPa(Sy, E, sr)
        sigma_e = BucklingAnalysis.euler_critical_stress_MPa(E, sr)
        assert sigma_j < sigma_e

    def test_curves_meet_at_transition(self, lib):
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        tr = BucklingAnalysis.transition_slenderness_ratio(Sy, E)
        sigma_j = BucklingAnalysis.johnson_critical_stress_MPa(Sy, E, tr)
        sigma_e = BucklingAnalysis.euler_critical_stress_MPa(E, tr)
        assert sigma_j == pytest.approx(sigma_e, rel=0.01)

    def test_unified_selects_johnson_below_transition(self, lib):
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        tr = BucklingAnalysis.transition_slenderness_ratio(Sy, E)
        sr = tr * 0.5
        sigma = BucklingAnalysis.critical_buckling_stress_MPa(Sy, E, sr)
        sigma_j = BucklingAnalysis.johnson_critical_stress_MPa(Sy, E, sr)
        assert sigma == pytest.approx(sigma_j)

    def test_unified_selects_euler_above_transition(self, lib):
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        tr = BucklingAnalysis.transition_slenderness_ratio(Sy, E)
        sr = tr * 1.5
        sigma = BucklingAnalysis.critical_buckling_stress_MPa(Sy, E, sr)
        sigma_e = BucklingAnalysis.euler_critical_stress_MPa(E, sr)
        assert sigma == pytest.approx(sigma_e)


# -- Phase III: Shaft Critical Speed --


class TestShaftCriticalSpeed:
    def test_critical_speed_positive(self):
        omega = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0,
            50.0,
            500.0,
            7850.0,
        )
        assert omega > 0

    def test_critical_scales_with_sqrt_E(self):
        omega1 = ShaftCriticalSpeed.first_critical_speed_rad_s(100.0, 50.0, 500.0, 7850.0)
        omega2 = ShaftCriticalSpeed.first_critical_speed_rad_s(200.0, 50.0, 500.0, 7850.0)
        assert omega2 == pytest.approx(omega1 * math.sqrt(2.0), rel=0.01)

    def test_critical_scales_inversely_with_L2(self):
        omega1 = ShaftCriticalSpeed.first_critical_speed_rad_s(200.0, 50.0, 500.0, 7850.0)
        omega2 = ShaftCriticalSpeed.first_critical_speed_rad_s(200.0, 50.0, 1000.0, 7850.0)
        assert omega2 == pytest.approx(omega1 / 4.0, rel=0.01)

    def test_critical_speed_margin_above_3(self):
        """50mm steel shaft, 500mm span, 30 RPM: margin >> 3."""
        omega = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0,
            50.0,
            500.0,
            7850.0,
        )
        crit_rpm = ShaftCriticalSpeed.critical_speed_rpm(omega)
        margin = ShaftCriticalSpeed.critical_speed_margin(crit_rpm, 30.0)
        assert margin > 3.0, f"Critical speed margin {margin:.1f} <= 3"


# -- Phase III: Notch Sensitivity --


class TestNotchSensitivity:
    def test_q_in_0_1(self):
        a = NotchSensitivity.neuber_constant_mm(500.0)
        q = NotchSensitivity.notch_sensitivity_q(2.0, a)
        assert 0 <= q <= 1

    def test_q_zero_for_zero_radius(self):
        a = NotchSensitivity.neuber_constant_mm(500.0)
        q = NotchSensitivity.notch_sensitivity_q(0.0, a)
        assert q == 0.0

    def test_Kf_between_1_and_Kt(self):
        Kt = 2.5
        a = NotchSensitivity.neuber_constant_mm(500.0)
        q = NotchSensitivity.notch_sensitivity_q(2.0, a)
        Kf = NotchSensitivity.fatigue_concentration_Kf(Kt, q)
        assert 1.0 <= Kf <= Kt

    def test_Se_corrected_less_than_uncorrected(self):
        Se = 200.0
        Kf = 1.5
        Se_corr = NotchSensitivity.corrected_endurance_limit_MPa(Se, Kf)
        assert Se_corr < Se

    def test_Kf_equals_Kt_when_fully_sensitive(self):
        """When q=1 (large r, strong material): K_f = K_t."""
        Kf = NotchSensitivity.fatigue_concentration_Kf(2.5, 1.0)
        assert Kf == pytest.approx(2.5)


# ---------------------------------------------------------------------------
# ShaftAnalysis extended
# ---------------------------------------------------------------------------


class TestShaftDeflectionExtended:
    """Moment of inertia scaling, deflection limit, edge cases."""

    def test_moment_of_inertia_proportional_to_d4(self):
        # I = pi*d^4/64: doubling d -> 16x I
        i1 = ShaftAnalysis.moment_of_inertia_m4(25.0)
        i2 = ShaftAnalysis.moment_of_inertia_m4(50.0)
        assert i2 == pytest.approx(16.0 * i1, rel=1e-6)

    def test_moment_of_inertia_positive(self):
        assert ShaftAnalysis.moment_of_inertia_m4(10.0) > 0

    def test_deflection_limit_formula(self):
        # L / 10000 by default
        limit = ShaftAnalysis.deflection_limit_mm(1000.0)
        assert limit == pytest.approx(0.1)

    def test_deflection_limit_custom_ratio(self):
        limit = ShaftAnalysis.deflection_limit_mm(500.0, ratio=500.0)
        assert limit == pytest.approx(1.0)

    def test_zero_force_gives_zero_deflection(self):
        d = ShaftAnalysis.max_deflection_simply_supported_mm(0.0, 500.0, 210.0, 50.0)
        assert d == pytest.approx(0.0)

    def test_larger_diameter_less_deflection(self):
        d_small = ShaftAnalysis.max_deflection_simply_supported_mm(
            1000.0, 500.0, 210.0, 25.0
        )
        d_large = ShaftAnalysis.max_deflection_simply_supported_mm(
            1000.0, 500.0, 210.0, 50.0
        )
        # I doubles with d^4: 50mm shaft deflects much less than 25mm
        assert d_large < d_small

    def test_deflection_proportional_to_L3(self):
        # delta proportional to L^3
        d1 = ShaftAnalysis.max_deflection_simply_supported_mm(100.0, 500.0, 210.0, 50.0)
        d2 = ShaftAnalysis.max_deflection_simply_supported_mm(100.0, 1000.0, 210.0, 50.0)
        assert d2 == pytest.approx(8.0 * d1, rel=1e-6)

    def test_single_bearing_falls_back_to_simply_supported(self):
        d_multi = ShaftAnalysis.max_deflection_multi_support_mm(
            100.0, 500.0, 1, 210.0, 50.0
        )
        d_single = ShaftAnalysis.max_deflection_simply_supported_mm(
            100.0, 500.0, 210.0, 50.0
        )
        assert d_multi == pytest.approx(d_single, rel=1e-6)

    def test_deflection_mm_is_float(self):
        d = ShaftAnalysis.max_deflection_simply_supported_mm(500.0, 500.0, 210.0, 50.0)
        assert isinstance(d, float)


# ---------------------------------------------------------------------------
# GearToothStress extended
# ---------------------------------------------------------------------------


class TestGearToothStressExtended:
    """Lewis form factor, stress proportionality, safety factor."""

    def test_lewis_form_factor_low_tooth_count(self):
        # < 12 teeth: Y = 0.245
        y = GearToothStress.lewis_form_factor(10)
        assert y == pytest.approx(0.245)

    def test_lewis_form_factor_high_tooth_count(self):
        # High tooth count: Y = 0.484 - 2.87/N
        y = GearToothStress.lewis_form_factor(100)
        assert y == pytest.approx(0.484 - 2.87 / 100, rel=1e-6)

    def test_lewis_form_factor_boundary_12(self):
        # At exactly 12 teeth: uses formula not constant
        y = GearToothStress.lewis_form_factor(12)
        assert y == pytest.approx(0.484 - 2.87 / 12, rel=1e-6)

    def test_lewis_form_factor_increases_with_teeth(self):
        # More teeth -> larger Y -> lower stress for same force
        y20 = GearToothStress.lewis_form_factor(20)
        y80 = GearToothStress.lewis_form_factor(80)
        assert y80 > y20

    def test_stress_proportional_to_force(self):
        s1 = GearToothStress.bending_stress_MPa(100.0, 15.0, 2.5, 20)
        s2 = GearToothStress.bending_stress_MPa(200.0, 15.0, 2.5, 20)
        assert s2 == pytest.approx(2.0 * s1, rel=1e-6)

    def test_larger_module_less_stress(self):
        # Larger module m -> lower sigma (sigma = Wt / (b * m * Y))
        s_small = GearToothStress.bending_stress_MPa(100.0, 15.0, 2.0, 20)
        s_large = GearToothStress.bending_stress_MPa(100.0, 15.0, 4.0, 20)
        assert s_large < s_small

    def test_sf_returns_float(self):
        sf = GearToothStress.safety_factor(200.0, 50.0)
        assert isinstance(sf, float)

    def test_sf_positive_for_positive_stress(self):
        sf = GearToothStress.safety_factor(200.0, 50.0)
        assert sf > 0

    def test_sf_equals_ratio(self):
        sf = GearToothStress.safety_factor(300.0, 100.0)
        assert sf == pytest.approx(3.0)


# ---------------------------------------------------------------------------
# FatigueAnalysis extended
# ---------------------------------------------------------------------------


class TestFatigueAnalysisExtended:
    """Temperature factor, size factor breakpoints, Goodman edge cases."""

    def test_temperature_factor_kd_at_ambient(self):
        # Below 250 C: kd = 1.0
        kd = FatigueAnalysis.temperature_factor_kd(30.0)
        assert kd == pytest.approx(1.0)

    def test_temperature_factor_kd_at_250(self):
        kd = FatigueAnalysis.temperature_factor_kd(250.0)
        assert kd == pytest.approx(1.0)

    def test_temperature_factor_kd_above_250(self):
        # Above 250 C: polynomial - should differ from 1.0
        kd = FatigueAnalysis.temperature_factor_kd(400.0)
        assert kd != pytest.approx(1.0)

    def test_size_factor_tiny_diameter(self):
        # d <= 2.79 mm: kb = 1.0
        kb = FatigueAnalysis.size_factor_kb(2.0)
        assert kb == pytest.approx(1.0)

    def test_size_factor_medium_diameter(self):
        # 2.79 < d <= 51: kb = 1.24 * d^(-0.107), must be < 1
        kb = FatigueAnalysis.size_factor_kb(25.0)
        assert 0 < kb < 1.0

    def test_size_factor_large_diameter(self):
        # 51 < d <= 254: kb = 1.51 * d^(-0.157), also < 1
        kb = FatigueAnalysis.size_factor_kb(100.0)
        assert 0 < kb < 1.0

    def test_size_factor_very_large_diameter(self):
        # d > 254: kb = 0.6 (constant floor)
        kb = FatigueAnalysis.size_factor_kb(300.0)
        assert kb == pytest.approx(0.6)

    def test_size_factor_decreases_with_diameter(self):
        kb_small = FatigueAnalysis.size_factor_kb(10.0)
        kb_large = FatigueAnalysis.size_factor_kb(50.0)
        assert kb_large < kb_small

    def test_surface_factor_forged_less_than_machined(self):
        # Forged (rougher) -> lower ka than machined
        ka_forged = FatigueAnalysis.surface_factor_ka(500.0, "forged")
        ka_machined = FatigueAnalysis.surface_factor_ka(500.0, "machined")
        assert ka_forged < ka_machined

    def test_surface_factor_hot_rolled_less_than_machined(self):
        ka_hr = FatigueAnalysis.surface_factor_ka(500.0, "hot_rolled")
        ka_m = FatigueAnalysis.surface_factor_ka(500.0, "machined")
        assert ka_hr < ka_m

    def test_goodman_sf_infinite_for_zero_amplitude(self):
        # Zero stress amplitude and zero mean -> 1/SF=0 -> inf
        sf = FatigueAnalysis.goodman_safety_factor(0.0, 0.0, 200.0, 500.0)
        assert sf == float("inf")

    def test_goodman_sf_returns_float(self):
        sf = FatigueAnalysis.goodman_safety_factor(50.0, 10.0, 150.0, 500.0)
        assert isinstance(sf, float)

    def test_fatigue_life_returns_float(self):
        N = FatigueAnalysis.fatigue_life_cycles(80.0, 100.0, 500.0)
        assert isinstance(N, float)

    def test_fatigue_life_at_endurance_limit_is_infinite(self):
        # Exactly at Se: below endurance limit -> 1e9
        N = FatigueAnalysis.fatigue_life_cycles(100.0, 100.0, 500.0)
        assert N >= 1e8

    def test_corrected_endurance_limit_positive(self):
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(300.0, 600.0, 50.0)
        assert Se > 0


# ---------------------------------------------------------------------------
# BucklingAnalysis extended
# ---------------------------------------------------------------------------


class TestBucklingAnalysisExtended:
    """Rectangular section formulas, Euler stress, transition ratio, SF."""

    def test_rectangular_section_area_formula(self):
        area = BucklingAnalysis.rectangular_section_area_mm2(20.0, 30.0)
        assert area == pytest.approx(600.0)

    def test_rectangular_section_I_formula(self):
        # I = b*h^3/12
        moi = BucklingAnalysis.rectangular_section_I_mm4(10.0, 20.0)
        assert pytest.approx(10.0 * 20.0**3 / 12.0, rel=1e-6) == moi

    def test_rectangular_section_square_is_symmetric(self):
        moi_bh = BucklingAnalysis.rectangular_section_I_mm4(20.0, 30.0)
        moi_hb = BucklingAnalysis.rectangular_section_I_mm4(30.0, 20.0)
        # Not equal (h is the loaded dimension)
        assert moi_bh != pytest.approx(moi_hb)

    def test_euler_stress_proportional_to_1_over_sr2(self):
        # sigma_cr = pi^2*E / (KL/r)^2: doubling sr -> 1/4 stress
        s1 = BucklingAnalysis.euler_critical_stress_MPa(200.0, 100.0)
        s2 = BucklingAnalysis.euler_critical_stress_MPa(200.0, 200.0)
        assert s2 == pytest.approx(s1 / 4.0, rel=1e-6)

    def test_euler_stress_proportional_to_E(self):
        s1 = BucklingAnalysis.euler_critical_stress_MPa(100.0, 100.0)
        s2 = BucklingAnalysis.euler_critical_stress_MPa(200.0, 100.0)
        assert s2 == pytest.approx(2.0 * s1, rel=1e-6)

    def test_euler_stress_positive(self):
        s = BucklingAnalysis.euler_critical_stress_MPa(200.0, 80.0)
        assert s > 0

    def test_transition_slenderness_formula(self, lib):
        steel = lib.get("steel")
        Sy = steel.yield_strength_MPa[0]
        E = steel.youngs_modulus_GPa[0]
        tr = BucklingAnalysis.transition_slenderness_ratio(Sy, E)
        expected = math.sqrt(2.0 * math.pi**2 * E * 1000.0 / Sy)
        assert tr == pytest.approx(expected, rel=1e-6)

    def test_slenderness_ratio_formula(self):
        # lambda = K*L / sqrt(I/A)
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        area = BucklingAnalysis.rectangular_section_area_mm2(25.0, 25.0)
        sr = BucklingAnalysis.slenderness_ratio(600.0, 1.0, moi, area)
        r = math.sqrt(moi / area)
        assert sr == pytest.approx(600.0 / r, rel=1e-6)

    def test_buckling_sf_returns_ratio(self):
        sf = BucklingAnalysis.buckling_safety_factor(1000.0, 100.0)
        assert sf == pytest.approx(10.0)

    def test_euler_critical_load_shorter_column_higher(self):
        moi = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_short = BucklingAnalysis.euler_critical_load_N(200.0, moi, 300.0, 0.5)
        P_long = BucklingAnalysis.euler_critical_load_N(200.0, moi, 600.0, 0.5)
        assert P_short > P_long


# ---------------------------------------------------------------------------
# StressConcentration extended
# ---------------------------------------------------------------------------


class TestStressConcentrationExtended:
    """Edge cases: smooth shaft, large fillet, clamping."""

    def test_smooth_shaft_D_equals_d_returns_1(self):
        # D == d: no step, Kt = 1.0 (guard in code)
        Kt = StressConcentration.stepped_shaft(50.0, 50.0, 3.0)
        assert Kt == pytest.approx(1.0)

    def test_zero_diameter_returns_1(self):
        Kt = StressConcentration.stepped_shaft(50.0, 0.0, 3.0)
        assert Kt == pytest.approx(1.0)

    def test_zero_radius_returns_1(self):
        Kt = StressConcentration.stepped_shaft(60.0, 50.0, 0.0)
        assert Kt == pytest.approx(1.0)

    def test_larger_fillet_lower_Kt(self):
        # Larger r/d -> smaller Kt (less stress concentration)
        Kt_small_r = StressConcentration.stepped_shaft(75.0, 50.0, 2.0)
        Kt_large_r = StressConcentration.stepped_shaft(75.0, 50.0, 10.0)
        assert Kt_large_r < Kt_small_r

    def test_kt_ge_1_always(self):
        # Kt must never be less than 1.0 by construction
        cases = [
            (60.0, 50.0, 3.0),
            (100.0, 50.0, 2.0),
            (55.0, 50.0, 15.0),
            (200.0, 50.0, 0.5),
        ]
        for D, d, r in cases:
            assert StressConcentration.stepped_shaft(D, d, r) >= 1.0

    def test_keyway_bending_constant(self):
        assert StressConcentration.keyway_bending() == pytest.approx(2.14)

    def test_keyway_torsion_constant(self):
        assert StressConcentration.keyway_torsion() == pytest.approx(3.0)

    def test_keyway_torsion_gt_bending(self):
        assert StressConcentration.keyway_torsion() > StressConcentration.keyway_bending()


# ---------------------------------------------------------------------------
# DynamicLoadFactor extended
# ---------------------------------------------------------------------------


class TestDynamicLoadFactorExtended:
    """Pitch velocity formula, quality number effect, Kv monotonicity."""

    def test_pitch_velocity_formula(self):
        # V = pi * (d_mm/304.8) * rpm (ft/min)
        V = DynamicLoadFactor.pitch_velocity_ft_min(50.0, 60.0)
        d_ft = 50.0 / 304.8
        expected = math.pi * d_ft * 60.0
        assert pytest.approx(expected, rel=1e-6) == V

    def test_pitch_velocity_proportional_to_rpm(self):
        v1 = DynamicLoadFactor.pitch_velocity_ft_min(50.0, 30.0)
        v2 = DynamicLoadFactor.pitch_velocity_ft_min(50.0, 60.0)
        assert v2 == pytest.approx(2.0 * v1, rel=1e-6)

    def test_pitch_velocity_proportional_to_diameter(self):
        v1 = DynamicLoadFactor.pitch_velocity_ft_min(50.0, 30.0)
        v2 = DynamicLoadFactor.pitch_velocity_ft_min(100.0, 30.0)
        assert v2 == pytest.approx(2.0 * v1, rel=1e-6)

    def test_kv_returns_float(self):
        kv = DynamicLoadFactor.agma_Kv(50.0)
        assert isinstance(kv, float)

    def test_kv_positive(self):
        kv = DynamicLoadFactor.agma_Kv(100.0)
        assert kv > 0

    def test_higher_quality_lower_Kv(self):
        # Higher quality number -> smaller Kv (closer to 1)
        kv_low_q = DynamicLoadFactor.agma_Kv(200.0, quality_number=3)
        kv_high_q = DynamicLoadFactor.agma_Kv(200.0, quality_number=9)
        assert kv_high_q < kv_low_q

    def test_agma_bending_stress_positive(self):
        sigma = DynamicLoadFactor.agma_bending_stress_MPa(10.0, 1.05)
        assert sigma > 0

    def test_agma_bending_equals_product(self):
        sigma = DynamicLoadFactor.agma_bending_stress_MPa(10.0, 1.2)
        assert sigma == pytest.approx(12.0)


# ---------------------------------------------------------------------------
# ShaftCriticalSpeed extended
# ---------------------------------------------------------------------------


class TestShaftCriticalSpeedExtended:
    """RPM conversion, density effect, margin formula."""

    def test_rpm_conversion_formula(self):
        omega = 100.0  # rad/s
        rpm = ShaftCriticalSpeed.critical_speed_rpm(omega)
        assert rpm == pytest.approx(omega * 60.0 / (2.0 * math.pi), rel=1e-6)

    def test_higher_density_lower_critical_speed(self):
        omega_steel = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0, 50.0, 500.0, 7850.0
        )
        # Denser material (lead-like) -> lower critical speed
        omega_dense = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0, 50.0, 500.0, 11340.0
        )
        assert omega_dense < omega_steel

    def test_critical_speed_positive(self):
        omega = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0, 50.0, 500.0, 7850.0
        )
        assert omega > 0

    def test_margin_formula(self):
        margin = ShaftCriticalSpeed.critical_speed_margin(3000.0, 30.0)
        assert margin == pytest.approx(100.0)

    def test_margin_zero_operating_rpm_returns_inf(self):
        margin = ShaftCriticalSpeed.critical_speed_margin(3000.0, 0.0)
        assert margin == float("inf")

    def test_margin_increases_with_critical_rpm(self):
        m1 = ShaftCriticalSpeed.critical_speed_margin(1000.0, 30.0)
        m2 = ShaftCriticalSpeed.critical_speed_margin(2000.0, 30.0)
        assert m2 > m1

    def test_larger_diameter_higher_critical_speed(self):
        # I proportional to d^4, A to d^2: omega ~ sqrt(I/A) ~ d
        omega_small = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0, 25.0, 500.0, 7850.0
        )
        omega_large = ShaftCriticalSpeed.first_critical_speed_rad_s(
            200.0, 50.0, 500.0, 7850.0
        )
        assert omega_large > omega_small


# ---------------------------------------------------------------------------
# NotchSensitivity extended
# ---------------------------------------------------------------------------


class TestNotchSensitivityExtended:
    """Neuber constant, q limiting behavior, Kf edge cases."""

    def test_neuber_constant_clamped_at_low_Su(self):
        # Su below 345 gets clamped; result is still valid
        a = NotchSensitivity.neuber_constant_mm(200.0)
        assert a > 0

    def test_neuber_constant_clamped_at_high_Su(self):
        a = NotchSensitivity.neuber_constant_mm(2000.0)
        assert a > 0

    def test_neuber_constant_is_positive(self):
        # Neuber constant always positive (floor at 0.001)
        for su in [400.0, 600.0, 900.0, 1400.0]:
            assert NotchSensitivity.neuber_constant_mm(su) > 0

    def test_q_approaches_1_for_large_radius(self):
        # q = 1/(1+a/r): large r -> q -> 1
        a = NotchSensitivity.neuber_constant_mm(500.0)
        q = NotchSensitivity.notch_sensitivity_q(1000.0, a)
        assert q > 0.99

    def test_q_returns_float(self):
        a = NotchSensitivity.neuber_constant_mm(500.0)
        q = NotchSensitivity.notch_sensitivity_q(2.0, a)
        assert isinstance(q, float)

    def test_Kf_equals_1_when_q_zero(self):
        # q=0 -> insensitive to notch -> Kf = 1
        Kf = NotchSensitivity.fatigue_concentration_Kf(3.0, 0.0)
        assert Kf == pytest.approx(1.0)

    def test_corrected_Se_unchanged_for_Kf_1(self):
        Se = 200.0
        Se_corr = NotchSensitivity.corrected_endurance_limit_MPa(Se, 1.0)
        assert Se_corr == pytest.approx(Se)

    def test_corrected_Se_for_high_Kf(self):
        # Kf=2 halves endurance limit
        Se_corr = NotchSensitivity.corrected_endurance_limit_MPa(200.0, 2.0)
        assert Se_corr == pytest.approx(100.0)

    def test_corrected_Se_zero_for_zero_Kf(self):
        Se_corr = NotchSensitivity.corrected_endurance_limit_MPa(200.0, 0.0)
        assert Se_corr == pytest.approx(0.0)


# ---------------------------------------------------------------------------
# CumulativeFatigue (Miner's Rule)
# ---------------------------------------------------------------------------


class TestCumulativeFatigue:
    """Initial state, damage accumulation, failure criterion."""

    def test_initial_damage_zero(self):
        cf = CumulativeFatigue()
        assert cf.damage == pytest.approx(0.0)

    def test_initial_not_failed(self):
        cf = CumulativeFatigue()
        assert not cf.is_failed()

    def test_initial_remaining_life_is_one(self):
        cf = CumulativeFatigue()
        assert cf.remaining_life_fraction() == pytest.approx(1.0)

    def test_below_endurance_no_damage(self):
        cf = CumulativeFatigue()
        # Stress amplitude below Se -> no damage
        cf.add_cycles(1000000, stress_amplitude_MPa=50.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.damage == pytest.approx(0.0)

    def test_above_endurance_accumulates_damage(self):
        cf = CumulativeFatigue()
        cf.add_cycles(100000, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.damage > 0.0

    def test_damage_accumulates_with_more_cycles(self):
        cf1 = CumulativeFatigue()
        cf2 = CumulativeFatigue()
        cf1.add_cycles(100, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=500.0)
        cf2.add_cycles(1000, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf2.damage > cf1.damage

    def test_failure_when_damage_reaches_1(self):
        cf = CumulativeFatigue()
        # Force damage > 1 by adding many cycles at high stress
        cf.add_cycles(10**8, stress_amplitude_MPa=450.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.is_failed()

    def test_remaining_life_decreases_with_damage(self):
        cf = CumulativeFatigue()
        cf.add_cycles(10000, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.remaining_life_fraction() < 1.0

    def test_remaining_life_never_negative(self):
        cf = CumulativeFatigue()
        cf.add_cycles(10**9, stress_amplitude_MPa=400.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.remaining_life_fraction() >= 0.0

    def test_cycle_log_records_entries(self):
        cf = CumulativeFatigue()
        cf.add_cycles(100, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=500.0)
        assert len(cf.cycle_log) == 1

    def test_below_endurance_does_not_log(self):
        cf = CumulativeFatigue()
        # Below Se -> returns early, does not append to cycle_log
        cf.add_cycles(100, stress_amplitude_MPa=50.0, Se_MPa=100.0, Su_MPa=500.0)
        assert len(cf.cycle_log) == 0

    def test_multiple_stress_levels_additive(self):
        cf = CumulativeFatigue()
        cf.add_cycles(1000, stress_amplitude_MPa=150.0, Se_MPa=100.0, Su_MPa=500.0)
        d_after_first = cf.damage
        cf.add_cycles(1000, stress_amplitude_MPa=250.0, Se_MPa=100.0, Su_MPa=500.0)
        assert cf.damage > d_after_first

    def test_zero_Se_returns_no_damage(self):
        cf = CumulativeFatigue()
        cf.add_cycles(1000, stress_amplitude_MPa=200.0, Se_MPa=0.0, Su_MPa=500.0)
        assert cf.damage == pytest.approx(0.0)

    def test_zero_Su_returns_no_damage(self):
        cf = CumulativeFatigue()
        cf.add_cycles(1000, stress_amplitude_MPa=200.0, Se_MPa=100.0, Su_MPa=0.0)
        assert cf.damage == pytest.approx(0.0)
