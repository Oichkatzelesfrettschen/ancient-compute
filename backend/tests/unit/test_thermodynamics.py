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
