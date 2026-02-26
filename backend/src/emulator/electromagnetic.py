"""
Electromagnetic Effects Model for Babbage Analytical Engine

Quantifies eddy current losses, galvanic corrosion risk, and static charge
accumulation. Expected conclusion: all effects negligible for a 19th-century
mechanical computer operating at 30 RPM in Earth's ambient magnetic field.

Key equations:
- Eddy current loss: P = (pi^2 * B^2 * d^2 * f^2 * V) / (6 * rho_e)
- At B=50 uT (Earth's field), f=0.5 Hz (30 RPM): P << 1 mW

Galvanic series (SCE reference):
  Brass: -0.30V, Steel: -0.60V, Bronze: -0.25V, Cast Iron: -0.55V
  Risk threshold: >0.25V potential difference
"""

from __future__ import annotations

import math
from dataclasses import dataclass

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

MU_0 = 4.0 * math.pi * 1e-7  # Vacuum permeability [H/m]
EARTH_B_FIELD_T = 50e-6       # Earth's magnetic field ~50 uT


# ---------------------------------------------------------------------------
# Galvanic Series (SCE reference electrode, volts)
# ---------------------------------------------------------------------------

# Electrode potentials vs Saturated Calomel Electrode (SCE)
GALVANIC_POTENTIAL_SCE_V: dict[str, float] = {
    "brass": -0.30,
    "steel": -0.60,
    "cast_iron": -0.55,
    "phosphor_bronze": -0.25,
    "spring_steel": -0.58,
}

# Risk thresholds for galvanic corrosion
GALVANIC_LOW_RISK_V = 0.15
GALVANIC_MODERATE_RISK_V = 0.25
GALVANIC_HIGH_RISK_V = 0.50


# ---------------------------------------------------------------------------
# Eddy Current Model
# ---------------------------------------------------------------------------

class EddyCurrentModel:
    """Eddy current losses in conducting bodies rotating in a magnetic field.

    P_eddy = (pi^2 * B^2 * d^2 * f^2 * V_vol) / (6 * rho_e)

    B: magnetic flux density [T]
    d: characteristic thickness [m] (e.g., shaft diameter)
    f: frequency of field variation [Hz] (= RPM/60 for rotation)
    V_vol: volume of conducting body [m^3]
    rho_e: electrical resistivity [ohm.m]
    """

    @staticmethod
    def eddy_loss_W(
        B_T: float,
        thickness_m: float,
        frequency_Hz: float,
        volume_m3: float,
        resistivity_ohm_m: float,
    ) -> float:
        """Compute eddy current power loss [W]."""
        if resistivity_ohm_m <= 0:
            return float("inf")
        return (
            math.pi**2 * B_T**2 * thickness_m**2 * frequency_Hz**2 * volume_m3
        ) / (6.0 * resistivity_ohm_m)

    @staticmethod
    def shaft_eddy_loss_W(
        diameter_mm: float,
        length_mm: float,
        rpm: float,
        resistivity_ohm_m: float,
        B_T: float = EARTH_B_FIELD_T,
    ) -> float:
        """Eddy current loss in a rotating shaft [W].

        Uses shaft diameter as characteristic thickness and RPM/60 as frequency.
        """
        d_m = diameter_mm / 1000.0
        l_m = length_mm / 1000.0
        volume = math.pi * (d_m / 2.0) ** 2 * l_m
        freq = rpm / 60.0
        return EddyCurrentModel.eddy_loss_W(B_T, d_m, freq, volume, resistivity_ohm_m)

    @staticmethod
    def gear_eddy_loss_W(
        pitch_diameter_mm: float,
        face_width_mm: float,
        rpm: float,
        resistivity_ohm_m: float,
        B_T: float = EARTH_B_FIELD_T,
    ) -> float:
        """Eddy current loss in a rotating gear [W].

        Approximates gear as solid disk.
        """
        d_m = pitch_diameter_mm / 1000.0
        fw_m = face_width_mm / 1000.0
        volume = math.pi * (d_m / 2.0) ** 2 * fw_m
        freq = rpm / 60.0
        return EddyCurrentModel.eddy_loss_W(B_T, fw_m, freq, volume, resistivity_ohm_m)


# ---------------------------------------------------------------------------
# Galvanic Corrosion Matrix
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class GalvanicPair:
    """A pair of metals in contact, with corrosion risk assessment."""
    material_anodic: str
    material_cathodic: str
    potential_difference_V: float
    risk_level: str  # "low", "moderate", "high"
    mitigation: str


class GalvanicCorrosionMatrix:
    """5x5 galvanic corrosion risk matrix for all material pairs."""

    def __init__(self, potentials: dict[str, float] | None = None) -> None:
        self._potentials = potentials or GALVANIC_POTENTIAL_SCE_V

    def potential_difference_V(self, mat_a: str, mat_b: str) -> float:
        """Absolute potential difference between two materials [V]."""
        va = self._potentials.get(mat_a, 0.0)
        vb = self._potentials.get(mat_b, 0.0)
        return abs(va - vb)

    def risk_level(self, mat_a: str, mat_b: str) -> str:
        """Classify galvanic corrosion risk: low, moderate, or high."""
        dv = self.potential_difference_V(mat_a, mat_b)
        if dv < GALVANIC_LOW_RISK_V:
            return "low"
        elif dv < GALVANIC_MODERATE_RISK_V:
            return "moderate"
        else:
            return "high"

    def mitigation(self, mat_a: str, mat_b: str) -> str:
        """Suggest mitigation for a galvanic pair."""
        risk = self.risk_level(mat_a, mat_b)
        if risk == "low":
            return "No action required."
        elif risk == "moderate":
            return "Apply oil film barrier; monitor during maintenance."
        else:
            return "Isolate with insulating washer or barrier coating; frequent inspection."

    def evaluate_pair(self, mat_a: str, mat_b: str) -> GalvanicPair:
        """Full evaluation of a material pair."""
        dv = self.potential_difference_V(mat_a, mat_b)
        # Determine which is anodic (more negative potential)
        va = self._potentials.get(mat_a, 0.0)
        vb = self._potentials.get(mat_b, 0.0)
        if va <= vb:
            anodic, cathodic = mat_a, mat_b
        else:
            anodic, cathodic = mat_b, mat_a
        return GalvanicPair(
            material_anodic=anodic,
            material_cathodic=cathodic,
            potential_difference_V=dv,
            risk_level=self.risk_level(mat_a, mat_b),
            mitigation=self.mitigation(mat_a, mat_b),
        )

    def full_matrix(self) -> list[GalvanicPair]:
        """Generate all unique pairs from the potential table."""
        names = sorted(self._potentials.keys())
        pairs = []
        for i, a in enumerate(names):
            for b in names[i + 1:]:
                pairs.append(self.evaluate_pair(a, b))
        return pairs


# ---------------------------------------------------------------------------
# Static Charge Model
# ---------------------------------------------------------------------------

class StaticChargeModel:
    """Triboelectric static charge accumulation estimate.

    For metal-on-metal contacts in an oiled environment, static charge
    is negligible. Oil film prevents charge separation. This model
    exists for completeness and to document the negligibility.
    """

    @staticmethod
    def triboelectric_charge_nC(
        contact_area_cm2: float,
        velocity_m_s: float,
        is_lubricated: bool = True,
    ) -> float:
        """Estimated triboelectric charge [nC].

        Metal-on-metal with oil: effectively zero.
        Metal-on-metal dry: ~0.01-0.1 nC/cm2 (negligible).
        """
        if is_lubricated:
            return 0.0
        # Dry metal-on-metal: very low charge density
        charge_density_nC_cm2 = 0.05
        return charge_density_nC_cm2 * contact_area_cm2

    @staticmethod
    def discharge_energy_J(charge_nC: float, capacitance_pF: float = 100.0) -> float:
        """Electrostatic discharge energy E = Q^2 / (2*C) [J].

        For sub-nC charges at ~100 pF, energy is << 1 uJ (negligible).
        """
        if capacitance_pF <= 0:
            return 0.0
        q_C = charge_nC * 1e-9
        c_F = capacitance_pF * 1e-12
        return q_C**2 / (2.0 * c_F)
