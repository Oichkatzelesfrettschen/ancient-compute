"""Tests for MaterialLibrary and MaterialProperties.

Validates:
- Schema loading and parsing
- All 5 materials accessible
- Property values within engineering handbook ranges
- SI unit conversions
- Safety factor calculations
- Error handling for missing materials
"""

import math
import pytest

pytestmark = pytest.mark.physics

from backend.src.emulator.materials import MaterialLibrary, MaterialProperties


@pytest.fixture
def lib():
    """Load the material library from the default schema."""
    return MaterialLibrary()


# -- Schema Loading --

class TestSchemaLoading:
    def test_loads_without_error(self, lib):
        assert lib is not None

    def test_has_five_materials(self, lib):
        assert len(lib) == 5

    def test_expected_material_names(self, lib):
        expected = {"brass", "cast_iron", "phosphor_bronze", "spring_steel", "steel"}
        assert set(lib.names()) == expected

    def test_all_materials_returns_list(self, lib):
        mats = lib.all_materials()
        assert len(mats) == 5
        assert all(isinstance(m, MaterialProperties) for m in mats)

    def test_contains_check(self, lib):
        assert "brass" in lib
        assert "titanium" not in lib

    def test_repr(self, lib):
        r = repr(lib)
        assert "MaterialLibrary" in r
        assert "5 materials" in r

    def test_missing_material_raises_key_error(self, lib):
        with pytest.raises(KeyError, match="Unknown material 'unobtanium'"):
            lib.get("unobtanium")

    def test_missing_schema_file_raises(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            MaterialLibrary(tmp_path / "nonexistent.yaml")

    def test_invalid_schema_raises(self, tmp_path):
        bad = tmp_path / "bad.yaml"
        bad.write_text("just_a_string")
        with pytest.raises(ValueError, match="missing 'materials'"):
            MaterialLibrary(bad)


# -- Individual Material Access --

class TestBrass:
    def test_accessible(self, lib):
        brass = lib.get("brass")
        assert brass.name == "brass"

    def test_designation(self, lib):
        assert "CZ121" in lib.get("brass").designation

    def test_density(self, lib):
        assert 8000 < lib.get("brass").density_kg_m3 < 9000

    def test_youngs_modulus_range(self, lib):
        e_min, e_max = lib.get("brass").youngs_modulus_GPa
        assert 80 <= e_min <= 120
        assert 80 <= e_max <= 120
        assert e_min <= e_max

    def test_yield_strength_range(self, lib):
        sy_min, sy_max = lib.get("brass").yield_strength_MPa
        assert 100 <= sy_min <= 400
        assert 100 <= sy_max <= 600

    def test_endurance_limit_positive(self, lib):
        se_min, se_max = lib.get("brass").endurance_limit_MPa
        assert se_min > 0
        assert se_max >= se_min

    def test_thermal_expansion(self, lib):
        alpha = lib.get("brass").thermal_expansion_coeff_per_K
        assert 15e-6 < alpha < 25e-6

    def test_nonmagnetic(self, lib):
        assert lib.get("brass").magnetic_permeability_relative < 2.0


class TestSteel:
    def test_accessible(self, lib):
        steel = lib.get("steel")
        assert steel.name == "steel"

    def test_density(self, lib):
        assert 7800 < lib.get("steel").density_kg_m3 < 7900

    def test_youngs_modulus_range(self, lib):
        e_min, e_max = lib.get("steel").youngs_modulus_GPa
        assert 195 <= e_min <= 215
        assert 195 <= e_max <= 215

    def test_yield_strength(self, lib):
        sy_min, _ = lib.get("steel").yield_strength_MPa
        assert sy_min >= 275

    def test_ferromagnetic(self, lib):
        assert lib.get("steel").magnetic_permeability_relative >= 50

    def test_thermal_expansion(self, lib):
        alpha = lib.get("steel").thermal_expansion_coeff_per_K
        assert 10e-6 < alpha < 14e-6


class TestCastIron:
    def test_accessible(self, lib):
        ci = lib.get("cast_iron")
        assert ci.name == "cast_iron"

    def test_density(self, lib):
        assert 7000 < lib.get("cast_iron").density_kg_m3 < 7500

    def test_has_compressive_strength(self, lib):
        ci = lib.get("cast_iron")
        assert ci.compressive_strength_MPa is not None
        assert ci.compressive_strength_MPa > ci.ultimate_tensile_strength_MPa[1]

    def test_poissons_ratio(self, lib):
        assert 0.20 < lib.get("cast_iron").poissons_ratio < 0.30

    def test_high_magnetic_permeability(self, lib):
        assert lib.get("cast_iron").magnetic_permeability_relative >= 100


class TestPhosphorBronze:
    def test_accessible(self, lib):
        pb = lib.get("phosphor_bronze")
        assert pb.name == "phosphor_bronze"

    def test_density(self, lib):
        assert 8500 < lib.get("phosphor_bronze").density_kg_m3 < 9000

    def test_nonmagnetic(self, lib):
        assert lib.get("phosphor_bronze").magnetic_permeability_relative < 2.0

    def test_low_friction(self, lib):
        assert lib.get("phosphor_bronze").friction_coeff <= 0.12

    def test_thermal_conductivity(self, lib):
        assert lib.get("phosphor_bronze").thermal_conductivity_W_mK > 50


class TestSpringSteel:
    def test_accessible(self, lib):
        ss = lib.get("spring_steel")
        assert ss.name == "spring_steel"

    def test_highest_endurance_limit(self, lib):
        ss = lib.get("spring_steel")
        for name in lib.names():
            other = lib.get(name)
            assert ss.endurance_limit_MPa[1] >= other.endurance_limit_MPa[0]

    def test_high_yield(self, lib):
        sy_min, sy_max = lib.get("spring_steel").yield_strength_MPa
        assert sy_min >= 500
        assert sy_max >= 1000


# -- Property Range Validation (all materials) --

class TestPropertyRanges:
    """Sanity checks that all materials have physically reasonable values."""

    def test_all_densities_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.density_kg_m3 > 0, f"{mat.name}: density must be positive"

    def test_all_densities_in_metal_range(self, lib):
        for mat in lib.all_materials():
            assert 1000 < mat.density_kg_m3 < 25000, f"{mat.name}: density out of range"

    def test_all_youngs_modulus_positive(self, lib):
        for mat in lib.all_materials():
            e_min, e_max = mat.youngs_modulus_GPa
            assert e_min > 0, f"{mat.name}: E_min must be positive"
            assert e_max >= e_min, f"{mat.name}: E_max < E_min"

    def test_all_poissons_ratio_in_range(self, lib):
        for mat in lib.all_materials():
            assert 0.0 < mat.poissons_ratio < 0.5, f"{mat.name}: nu out of [0, 0.5)"

    def test_all_yield_below_uts(self, lib):
        for mat in lib.all_materials():
            assert mat.yield_strength_MPa[0] <= mat.ultimate_tensile_strength_MPa[1], (
                f"{mat.name}: Sy_min > Su_max"
            )

    def test_all_endurance_below_uts(self, lib):
        for mat in lib.all_materials():
            assert mat.endurance_limit_MPa[1] <= mat.ultimate_tensile_strength_MPa[1], (
                f"{mat.name}: Se_max > Su_max"
            )

    def test_all_friction_positive(self, lib):
        for mat in lib.all_materials():
            assert 0.0 < mat.friction_coeff < 1.0, f"{mat.name}: friction out of range"

    def test_all_thermal_expansion_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.thermal_expansion_coeff_per_K > 0, f"{mat.name}: CTE must be positive"

    def test_all_specific_heat_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.specific_heat_J_kgK > 100, f"{mat.name}: c_p too low"

    def test_all_thermal_conductivity_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.thermal_conductivity_W_mK > 0, f"{mat.name}: k must be positive"

    def test_all_hardness_positive(self, lib):
        for mat in lib.all_materials():
            hb_min, hb_max = mat.hardness_HB
            assert hb_min > 0, f"{mat.name}: hardness must be positive"
            assert hb_max >= hb_min, f"{mat.name}: HB_max < HB_min"

    def test_all_resistivity_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.electrical_resistivity_ohm_m > 0, f"{mat.name}: rho_e must be positive"

    def test_all_permeability_positive(self, lib):
        for mat in lib.all_materials():
            assert mat.magnetic_permeability_relative >= 1.0, f"{mat.name}: mu_r < 1"

    def test_all_creep_threshold_above_ambient(self, lib):
        for mat in lib.all_materials():
            assert mat.creep_threshold_C > 40, f"{mat.name}: creep threshold below ambient"

    def test_temperature_range_covers_ambient(self, lib):
        for mat in lib.all_materials():
            t_min, t_max = mat.temperature_range_C
            assert t_min <= 20 <= t_max, f"{mat.name}: temp range doesn't cover 20C"


# -- SI Unit Conversions --

class TestUnitConversions:
    def test_youngs_modulus_pa(self, lib):
        brass = lib.get("brass")
        e_pa_min, e_pa_max = brass.youngs_modulus_Pa
        assert e_pa_min == brass.youngs_modulus_GPa[0] * 1e9
        assert e_pa_max == brass.youngs_modulus_GPa[1] * 1e9

    def test_yield_strength_pa(self, lib):
        steel = lib.get("steel")
        sy_pa_min, sy_pa_max = steel.yield_strength_Pa
        assert sy_pa_min == steel.yield_strength_MPa[0] * 1e6

    def test_shear_modulus(self, lib):
        steel = lib.get("steel")
        g_min, g_max = steel.shear_modulus_GPa
        expected_min = steel.youngs_modulus_GPa[0] / (2 * (1 + steel.poissons_ratio))
        assert abs(g_min - expected_min) < 0.01


# -- Safety Factors --

class TestSafetyFactors:
    def test_yield_sf_normal_case(self, lib):
        steel = lib.get("steel")
        sf = steel.safety_factor_yield(100.0)
        assert sf == pytest.approx(275.0 / 100.0, rel=0.01)

    def test_yield_sf_zero_stress(self, lib):
        sf = lib.get("steel").safety_factor_yield(0.0)
        assert sf == float("inf")

    def test_fatigue_sf_normal_case(self, lib):
        brass = lib.get("brass")
        sf = brass.safety_factor_fatigue(50.0)
        assert sf == pytest.approx(90.0 / 50.0, rel=0.01)

    def test_fatigue_sf_zero_stress(self, lib):
        sf = lib.get("brass").safety_factor_fatigue(0.0)
        assert sf == float("inf")
