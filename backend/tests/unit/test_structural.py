"""Tests for Structural Analysis Module.

Validates:
- Shaft deflection within L/10000 limit
- Gear tooth SF >= 2.0
- Fatigue life > 10^8 cycles
- Buckling SF >= 3.0 for vertical supports
"""

import math
import pytest

from backend.src.emulator.structural import (
    ShaftAnalysis,
    GearToothStress,
    FatigueAnalysis,
    BucklingAnalysis,
)
from backend.src.emulator.materials import MaterialLibrary


@pytest.fixture
def lib():
    return MaterialLibrary()


# -- Shaft Deflection --

class TestShaftDeflection:
    def test_deflection_positive(self):
        d = ShaftAnalysis.max_deflection_simply_supported_mm(
            1000.0, 500.0, 200.0, 50.0,
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
            load, 1500.0, 4, steel.youngs_modulus_GPa[0], 50.0,
        )
        limit = ShaftAnalysis.deflection_limit_mm(1500.0 / 3)  # span between bearings
        assert d < limit, f"Deflection {d:.4f} mm >= limit {limit:.4f} mm"

    def test_moment_of_inertia_positive(self):
        I = ShaftAnalysis.moment_of_inertia_m4(50.0)
        assert I > 0


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
            10.0, 0.0, Se, steel.ultimate_tensile_strength_MPa[0],
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
            10.0, Se, steel.ultimate_tensile_strength_MPa[0],
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
        I = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P = BucklingAnalysis.euler_critical_load_N(200.0, I, 600.0, 0.5)
        assert P > 0

    def test_slenderness_ratio_positive(self):
        I = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        A = BucklingAnalysis.rectangular_section_area_mm2(25.0, 25.0)
        sr = BucklingAnalysis.slenderness_ratio(600.0, 0.5, I, A)
        assert sr > 0

    def test_column_support_sf_above_3(self, lib):
        """Vertical digit column support: 25x25mm steel, 600mm, fixed-fixed."""
        steel = lib.get("steel")
        w = 25.0
        h = 25.0
        I = BucklingAnalysis.rectangular_section_I_mm4(w, h)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0], I, 600.0, 0.5,
        )
        # Applied load: weight of column mechanism (~5 kg per column)
        applied = 5.0 * 9.81
        sf = BucklingAnalysis.buckling_safety_factor(P_cr, applied)
        assert sf >= 3.0, f"Buckling SF={sf:.1f} < 3.0"

    def test_longer_column_lower_critical_load(self):
        I = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_short = BucklingAnalysis.euler_critical_load_N(200.0, I, 300.0, 0.5)
        P_long = BucklingAnalysis.euler_critical_load_N(200.0, I, 600.0, 0.5)
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
            23.0 * 9.81, 1500.0, 4, steel.youngs_modulus_GPa[0], 50.0,
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
            10.0, 0.0, Se, steel.ultimate_tensile_strength_MPa[0],
        )
        assert sf_fatigue >= 2.0

        # Buckling
        I = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0], I, 600.0, 0.5,
        )
        sf_buckling = BucklingAnalysis.buckling_safety_factor(P_cr, 5.0 * 9.81)
        assert sf_buckling >= 3.0
