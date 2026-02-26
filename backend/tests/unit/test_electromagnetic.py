"""Tests for Electromagnetic Effects Model.

Validates:
- Eddy current losses are negligible (< 1 mW for Earth's field at 30 RPM)
- Galvanic corrosion matrix classifies all pairs correctly
- Static charge is negligible in lubricated conditions
- EM losses << friction losses
"""

import math
import pytest

pytestmark = pytest.mark.physics

from backend.src.emulator.electromagnetic import (
    EddyCurrentModel,
    GalvanicCorrosionMatrix,
    StaticChargeModel,
    EARTH_B_FIELD_T,
)
from backend.src.emulator.materials import MaterialLibrary


@pytest.fixture
def lib():
    return MaterialLibrary()


@pytest.fixture
def galvanic():
    return GalvanicCorrosionMatrix()


# -- Eddy Current Losses --

class TestEddyCurrentLoss:
    def test_shaft_loss_negligible(self, lib):
        steel = lib.get("steel")
        p = EddyCurrentModel.shaft_eddy_loss_W(
            diameter_mm=50, length_mm=1500, rpm=30,
            resistivity_ohm_m=steel.electrical_resistivity_ohm_m,
        )
        assert p < 0.001, f"Shaft eddy loss {p*1000:.3f} mW should be << 1 mW"

    def test_gear_loss_negligible(self, lib):
        brass = lib.get("brass")
        p = EddyCurrentModel.gear_eddy_loss_W(
            pitch_diameter_mm=150, face_width_mm=15, rpm=30,
            resistivity_ohm_m=brass.electrical_resistivity_ohm_m,
        )
        assert p < 0.001, f"Gear eddy loss {p*1000:.3f} mW should be << 1 mW"

    def test_loss_proportional_to_rpm_squared(self, lib):
        steel = lib.get("steel")
        p30 = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, steel.electrical_resistivity_ohm_m,
        )
        p60 = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 60, steel.electrical_resistivity_ohm_m,
        )
        # P proportional to f^2, so doubling RPM -> 4x loss
        assert p60 == pytest.approx(4.0 * p30, rel=0.01)

    def test_loss_positive(self, lib):
        steel = lib.get("steel")
        p = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, steel.electrical_resistivity_ohm_m,
        )
        assert p > 0

    def test_higher_permeability_same_formula(self, lib):
        # Cast iron has higher mu_r but this formula doesn't directly
        # use mu_r (it's the loss in the conductor from an external field).
        # Higher resistivity -> lower loss.
        ci = lib.get("cast_iron")
        steel = lib.get("steel")
        p_ci = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, ci.electrical_resistivity_ohm_m,
        )
        p_steel = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, steel.electrical_resistivity_ohm_m,
        )
        # Cast iron has higher resistivity -> lower eddy loss
        assert p_ci < p_steel

    def test_total_em_loss_negligible(self, lib):
        """Sum of all eddy losses should be << 1 W (negligible vs friction)."""
        steel = lib.get("steel")
        brass = lib.get("brass")
        total = 0.0
        # Main shaft
        total += EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, steel.electrical_resistivity_ohm_m,
        )
        # 4 gears (2 stages x 2 gears)
        for pd_mm in [50, 150, 75, 150]:
            total += EddyCurrentModel.gear_eddy_loss_W(
                pd_mm, 15, 30, brass.electrical_resistivity_ohm_m,
            )
        assert total < 0.01, f"Total EM loss {total*1000:.3f} mW should be << 10 mW"


# -- Galvanic Corrosion Matrix --

class TestGalvanicCorrosion:
    def test_brass_steel_moderate(self, galvanic):
        risk = galvanic.risk_level("brass", "steel")
        assert risk in ("moderate", "high"), f"Brass-steel should be moderate/high, got {risk}"

    def test_brass_bronze_low(self, galvanic):
        risk = galvanic.risk_level("brass", "phosphor_bronze")
        assert risk == "low"

    def test_steel_cast_iron_low(self, galvanic):
        # Steel: -0.60V, Cast iron: -0.55V -> 0.05V difference -> low
        risk = galvanic.risk_level("steel", "cast_iron")
        assert risk == "low"

    def test_steel_is_anodic_to_brass(self, galvanic):
        pair = galvanic.evaluate_pair("brass", "steel")
        assert pair.material_anodic == "steel"
        assert pair.material_cathodic == "brass"

    def test_potential_difference_positive(self, galvanic):
        for pair in galvanic.full_matrix():
            assert pair.potential_difference_V >= 0

    def test_full_matrix_has_10_pairs(self, galvanic):
        # C(5,2) = 10 unique pairs
        pairs = galvanic.full_matrix()
        assert len(pairs) == 10

    def test_all_pairs_have_mitigation(self, galvanic):
        for pair in galvanic.full_matrix():
            assert pair.mitigation
            assert pair.risk_level in ("low", "moderate", "high")

    def test_symmetric(self, galvanic):
        dv_ab = galvanic.potential_difference_V("brass", "steel")
        dv_ba = galvanic.potential_difference_V("steel", "brass")
        assert dv_ab == dv_ba


# -- Static Charge --

class TestStaticCharge:
    def test_lubricated_zero_charge(self):
        q = StaticChargeModel.triboelectric_charge_nC(10.0, 0.1, is_lubricated=True)
        assert q == 0.0

    def test_dry_positive_charge(self):
        q = StaticChargeModel.triboelectric_charge_nC(10.0, 0.1, is_lubricated=False)
        assert q > 0

    def test_dry_charge_negligible(self):
        q = StaticChargeModel.triboelectric_charge_nC(100.0, 1.0, is_lubricated=False)
        assert q < 10.0, "Charge should be < 10 nC for metal-on-metal"

    def test_discharge_energy_negligible(self):
        q = StaticChargeModel.triboelectric_charge_nC(100.0, 1.0, is_lubricated=False)
        e = StaticChargeModel.discharge_energy_J(q)
        assert e < 1e-6, f"Discharge energy {e:.2e} J should be << 1 uJ"


# -- Cross-validation: EM losses << friction losses --

class TestEMvsfriction:
    def test_eddy_losses_fraction_of_friction(self, lib):
        """Eddy losses should be < 0.001% of estimated friction (~10-50 W)."""
        steel = lib.get("steel")
        p_eddy = EddyCurrentModel.shaft_eddy_loss_W(
            50, 1500, 30, steel.electrical_resistivity_ohm_m,
        )
        friction_estimate_W = 10.0  # Conservative lower bound
        ratio = p_eddy / friction_estimate_W
        assert ratio < 1e-4, f"Eddy/friction ratio {ratio:.2e} too high"
