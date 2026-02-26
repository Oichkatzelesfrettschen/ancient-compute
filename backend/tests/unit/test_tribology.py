"""Tests for Tribology and Wear Model.

Validates:
- Archard wear volume positive
- PV products below limits
- Film thickness positive
- Lambda ratios > 3 for full-film lubrication
- Cross-validation with lubrication schedule
"""

import math
import pytest

from backend.src.emulator.tribology import (
    WearModel,
    PVAnalysis,
    LubricationModel,
    SurfaceFinishSpec,
    PERIOD_SURFACE_FINISHES,
    ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
    ARCHARD_K_LUBRICATED_BRASS_ON_STEEL,
    PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED,
)
from backend.src.emulator.materials import MaterialLibrary


@pytest.fixture
def lib():
    return MaterialLibrary()


# -- Archard Wear --

class TestArchardWear:
    def test_volume_positive(self):
        v = WearModel.archard_volume_mm3(1e-6, 1000.0, 1000.0, 1000.0)
        assert v > 0

    def test_volume_proportional_to_force(self):
        v1 = WearModel.archard_volume_mm3(1e-6, 500.0, 1000.0, 1000.0)
        v2 = WearModel.archard_volume_mm3(1e-6, 1000.0, 1000.0, 1000.0)
        assert v2 == pytest.approx(2.0 * v1)

    def test_harder_material_less_wear(self):
        v_soft = WearModel.archard_volume_mm3(1e-6, 1000.0, 1000.0, 500.0)
        v_hard = WearModel.archard_volume_mm3(1e-6, 1000.0, 1000.0, 1000.0)
        assert v_hard < v_soft

    def test_hb_to_mpa_conversion(self):
        mpa = WearModel.hardness_HB_to_MPa(100)
        assert mpa == pytest.approx(981.0)

    def test_sliding_distance_positive(self):
        s = WearModel.sliding_distance_per_hour_mm(50.0, 30.0)
        assert s > 0

    def test_bearing_wear_per_hour(self, lib):
        pb = lib.get("phosphor_bronze")
        v = WearModel.bearing_wear_volume_per_hour_mm3(
            K=ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
            radial_load_N=1000.0,
            shaft_diameter_mm=50.0,
            rpm=30.0,
            hardness_HB=pb.hardness_HB[0],
        )
        assert v > 0
        # Wear should be very small with lubrication: < 1 mm^3/hr
        assert v < 1.0

    def test_gear_wear_per_hour(self, lib):
        brass = lib.get("brass")
        v = WearModel.gear_wear_volume_per_hour_mm3(
            K=ARCHARD_K_LUBRICATED_BRASS_ON_STEEL,
            tangential_force_N=100.0,
            pitch_diameter_mm=150.0,
            rpm=10.0,
            tooth_count=60,
            hardness_HB=brass.hardness_HB[0],
        )
        assert v > 0
        assert v < 1.0


# -- PV Analysis --

class TestPVAnalysis:
    def test_pressure_positive(self):
        p = PVAnalysis.bearing_pressure_MPa(1000.0, 50.0, 60.0)
        assert p > 0

    def test_velocity_positive(self):
        v = PVAnalysis.surface_velocity_m_s(50.0, 30.0)
        assert v > 0

    def test_pv_below_limit(self):
        """Main shaft bearings at 30 RPM should have PV well below limit."""
        # 4 bearings, 500 kg machine -> ~1225 N per bearing
        load = 500 * 9.81 / 4
        pv = PVAnalysis.pv_product_MPa_m_s(load, 50.0, 60.0, 30.0)
        assert PVAnalysis.is_within_limit(pv), (
            f"PV={pv:.3f} exceeds limit {PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED}"
        )

    def test_pv_proportional_to_rpm(self):
        pv30 = PVAnalysis.pv_product_MPa_m_s(1000.0, 50.0, 60.0, 30.0)
        pv60 = PVAnalysis.pv_product_MPa_m_s(1000.0, 50.0, 60.0, 60.0)
        assert pv60 == pytest.approx(2.0 * pv30, rel=0.01)


# -- Lubrication Film --

class TestLubricationFilm:
    def test_film_thickness_positive(self):
        h = LubricationModel.minimum_film_thickness_um(
            viscosity_Pa_s=0.059,
            entrainment_velocity_m_s=0.08,
            effective_radius_mm=25.0,
            reduced_modulus_GPa=110.0,
            load_per_length_N_mm=20.0,
        )
        assert h > 0

    def test_lambda_ratio_full_film(self):
        # With reasonable film thickness and 19th-c surface finish
        lam = LubricationModel.lambda_ratio(5.0, 0.8, 0.8)
        assert lam > 3.0, f"Lambda {lam:.1f} should be > 3 for full-film"

    def test_lambda_ratio_zero_roughness(self):
        lam = LubricationModel.lambda_ratio(5.0, 0.0, 0.0)
        assert lam == float("inf")

    def test_regime_classification(self):
        assert LubricationModel.regime(4.0) == "full_film"
        assert LubricationModel.regime(2.0) == "mixed"
        assert LubricationModel.regime(0.5) == "boundary"


# -- Surface Finish --

class TestSurfaceFinish:
    def test_period_finishes_defined(self):
        assert len(PERIOD_SURFACE_FINISHES) >= 4

    def test_all_ra_positive(self):
        for spec in PERIOD_SURFACE_FINISHES:
            assert spec.Ra_um > 0

    def test_journal_finest(self):
        journal = [s for s in PERIOD_SURFACE_FINISHES if "journal" in s.component][0]
        assert journal.Ra_um <= 0.8


# -- Cross-validation with lubrication schedule --

class TestLubricationSchedule:
    def test_wear_compatible_with_160h_schedule(self, lib):
        """Bearing wear over 160 hours should be << 1 mm depth."""
        pb = lib.get("phosphor_bronze")
        v_per_hr = WearModel.bearing_wear_volume_per_hour_mm3(
            K=ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
            radial_load_N=500 * 9.81 / 4,
            shaft_diameter_mm=50.0,
            rpm=30.0,
            hardness_HB=pb.hardness_HB[0],
        )
        v_160h = v_per_hr * 160.0
        # Convert volume to depth: depth = V / (pi * d * L)
        area_mm2 = math.pi * 50.0 * 60.0  # bearing contact area
        depth_mm = v_160h / area_mm2
        assert depth_mm < 0.01, f"Wear depth {depth_mm:.4f} mm over 160h too high"
