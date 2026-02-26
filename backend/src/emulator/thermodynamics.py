"""
Thermodynamic Model for Babbage Analytical Engine

Quantifies heat generation from friction, thermal expansion effects on
clearances, operating temperature envelope, and thermal time constants.

Key equations (Shigley Ch.12-13):
- Bearing heat: Q = 0.5 * mu * W * d * omega
- Gear mesh heat: Q = P * (1 - eta)
- Thermal expansion: delta_L = alpha * L * delta_T
- Dissimilar-metal clearance: delta = (alpha_1*L_1 - alpha_2*L_2) * delta_T
- Thermal time constant: tau = m * c_p / (h * A)
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import yaml

from backend.src.emulator.materials import MaterialLibrary, MaterialProperties


# ---------------------------------------------------------------------------
# Friction Heat Model (Shigley Ch.12-13)
# ---------------------------------------------------------------------------

@dataclass
class HeatSource:
    """A single heat source with its contribution in watts."""
    name: str
    heat_W: float
    source_type: str  # "bearing", "gear", "cam"


class FrictionHeatModel:
    """Computes friction heat generation from bearings, gears, and cams."""

    @staticmethod
    def bearing_heat_W(
        friction_coeff: float,
        radial_load_N: float,
        shaft_diameter_mm: float,
        angular_velocity_rad_s: float,
    ) -> float:
        """Bearing friction heat Q = 0.5 * mu * W * d * omega [W].

        Shigley Eq.12-7 (journal bearing friction power loss).
        d in meters, W in N, omega in rad/s.
        """
        d_m = shaft_diameter_mm / 1000.0
        return 0.5 * friction_coeff * radial_load_N * d_m * angular_velocity_rad_s

    @staticmethod
    def gear_mesh_heat_W(transmitted_power_W: float, efficiency: float) -> float:
        """Gear mesh heat loss Q = P * (1 - eta) [W].

        eta is mesh efficiency (typically 0.97-0.99 per stage for
        well-lubricated spur gears).
        """
        return transmitted_power_W * (1.0 - efficiency)

    @staticmethod
    def cam_friction_heat_W(
        friction_coeff: float,
        normal_force_N: float,
        sliding_velocity_m_s: float,
    ) -> float:
        """Cam-follower friction heat Q = mu * F_N * v [W]."""
        return friction_coeff * normal_force_N * sliding_velocity_m_s


# ---------------------------------------------------------------------------
# Thermal Expansion Model
# ---------------------------------------------------------------------------

class ThermalExpansionModel:
    """Thermal expansion effects on clearances and fits."""

    @staticmethod
    def linear_expansion_mm(
        alpha_per_K: float,
        length_mm: float,
        delta_T_K: float,
    ) -> float:
        """delta_L = alpha * L * delta_T [mm]."""
        return alpha_per_K * length_mm * delta_T_K

    @staticmethod
    def dissimilar_metal_clearance_change_mm(
        alpha_1_per_K: float,
        length_1_mm: float,
        alpha_2_per_K: float,
        length_2_mm: float,
        delta_T_K: float,
    ) -> float:
        """Clearance change between two dissimilar metals.

        delta = (alpha_1 * L_1 - alpha_2 * L_2) * delta_T [mm].
        Positive means clearance increases.
        """
        return (alpha_1_per_K * length_1_mm - alpha_2_per_K * length_2_mm) * delta_T_K

    @staticmethod
    def gear_backlash_change_mm(
        alpha_gear: float,
        alpha_shaft: float,
        center_distance_mm: float,
        delta_T_K: float,
    ) -> float:
        """Change in gear backlash due to differential expansion.

        If gear expands more than shaft/housing, backlash decreases.
        delta_backlash = (alpha_gear - alpha_shaft) * c * delta_T [mm].
        """
        return (alpha_gear - alpha_shaft) * center_distance_mm * delta_T_K

    @staticmethod
    def bearing_clearance_change_mm(
        alpha_bushing: float,
        alpha_shaft: float,
        bore_diameter_mm: float,
        delta_T_K: float,
    ) -> float:
        """Change in bearing radial clearance.

        If bushing expands more than shaft, clearance increases.
        delta = (alpha_bushing - alpha_shaft) * d * delta_T [mm].
        """
        return (alpha_bushing - alpha_shaft) * bore_diameter_mm * delta_T_K


# ---------------------------------------------------------------------------
# Operating Envelope
# ---------------------------------------------------------------------------

@dataclass
class OperatingEnvelope:
    """Thermal operating envelope for the machine."""
    ambient_T_min_C: float = 10.0
    ambient_T_max_C: float = 40.0
    total_heat_generation_W: float = 0.0
    heat_sources: List[HeatSource] = field(default_factory=list)
    steady_state_rise_C: float = 0.0
    thermal_time_constant_s: float = 0.0

    @property
    def operating_T_min_C(self) -> float:
        return self.ambient_T_min_C

    @property
    def operating_T_max_C(self) -> float:
        return self.ambient_T_max_C + self.steady_state_rise_C

    @property
    def thermal_time_constant_min(self) -> float:
        return self.thermal_time_constant_s / 60.0


def compute_thermal_time_constant_s(
    mass_kg: float,
    specific_heat_J_kgK: float,
    surface_area_m2: float,
    h_conv_W_m2K: float = 10.0,
) -> float:
    """Thermal time constant tau = m * c_p / (h * A) [s].

    h_conv: natural convection coefficient (~5-15 W/m2K for free
    convection in still air; 10 is a reasonable mid-range).
    """
    if surface_area_m2 <= 0 or h_conv_W_m2K <= 0:
        return float("inf")
    return mass_kg * specific_heat_J_kgK / (h_conv_W_m2K * surface_area_m2)


def compute_steady_state_rise_C(
    total_heat_W: float,
    surface_area_m2: float,
    h_conv_W_m2K: float = 10.0,
) -> float:
    """Steady-state temperature rise delta_T = Q / (h * A) [K or C]."""
    if surface_area_m2 <= 0 or h_conv_W_m2K <= 0:
        return float("inf")
    return total_heat_W / (h_conv_W_m2K * surface_area_m2)


# ---------------------------------------------------------------------------
# Full Engine Thermal Analysis
# ---------------------------------------------------------------------------

def compute_engine_thermal_model(
    schema_path: Optional[str] = None,
) -> OperatingEnvelope:
    """Build a complete thermal model from sim_schema.yaml parameters.

    Computes heat from 4 bearings, 2 gear stages, and 2 cam followers.
    Returns an OperatingEnvelope with all results.
    """
    path = Path(schema_path) if schema_path else (
        Path(__file__).resolve().parents[3] / "docs" / "simulation" / "sim_schema.yaml"
    )
    with path.open("r", encoding="utf-8") as fh:
        data = yaml.safe_load(fh)

    lib = MaterialLibrary(path)
    mech = data.get("mechanisms", {})
    drive = mech.get("drive", {})
    rpm = float(drive.get("rpm", 30))
    omega = 2.0 * math.pi * rpm / 60.0
    total_power_W = float(drive.get("estimated_power_W", 500))

    envelope = OperatingEnvelope()
    sources: List[HeatSource] = []

    # -- Bearings --
    bearings = mech.get("bearings", {})
    n_bearings = int(bearings.get("count", 4))
    shaft_d_mm = float(bearings.get("bore_diameter_mm", 50))
    machine_mass = float(data.get("structural", {}).get("machine_mass_kg", 500))
    # Approximate radial load per bearing: machine weight / bearing count
    radial_load_per_bearing_N = machine_mass * 9.81 / n_bearings

    pb = lib.get("phosphor_bronze")
    mu_bearing = pb.friction_coeff

    for i in range(n_bearings):
        q = FrictionHeatModel.bearing_heat_W(
            mu_bearing, radial_load_per_bearing_N, shaft_d_mm, omega
        )
        sources.append(HeatSource(f"bearing_{i}", q, "bearing"))

    # -- Gear mesh --
    gear_train = mech.get("gear_train", {})
    efficiency = float(gear_train.get("total_efficiency", 0.85))
    # Per-stage efficiency: sqrt(total) for 2 stages
    eta_per_stage = math.sqrt(efficiency)
    transmitted = total_power_W
    for i, stage in enumerate(gear_train.get("stages", [])):
        q = FrictionHeatModel.gear_mesh_heat_W(transmitted, eta_per_stage)
        sources.append(HeatSource(f"gear_stage_{i}", q, "gear"))
        transmitted *= eta_per_stage

    # -- Cam followers --
    cams = mech.get("cam_profiles", {})
    steel = lib.get("steel")
    for cam_name, cam_data in cams.items():
        lift_mm = float(cam_data.get("total_lift_mm", 5))
        spring_rate = float(cam_data.get("spring_rate_N_mm", 50))
        # Approximate: average normal force = spring_rate * lift/2
        avg_force = spring_rate * lift_mm / 2.0
        # Approximate sliding velocity from cam geometry
        rise_angle = float(cam_data.get("rise_angle_deg", 60))
        sliding_v = (lift_mm / 1000.0) / (rise_angle / 360.0 / (rpm / 60.0))
        q = FrictionHeatModel.cam_friction_heat_W(steel.friction_coeff, avg_force, sliding_v)
        sources.append(HeatSource(cam_name, q, "cam"))

    envelope.heat_sources = sources
    envelope.total_heat_generation_W = sum(s.heat_W for s in sources)

    # -- Thermal time constant and steady-state rise --
    # Approximate surface area: 2m x 0.8m x 1m box -> ~7.2 m2
    footprint = data.get("structural", {}).get("footprint_m", [2.0, 0.8])
    height_m = 1.0  # Approximate
    surface_area = 2 * (footprint[0] * footprint[1] + footprint[0] * height_m + footprint[1] * height_m)

    # Weighted average c_p (approximate as cast iron frame)
    ci = lib.get("cast_iron")
    envelope.thermal_time_constant_s = compute_thermal_time_constant_s(
        machine_mass, ci.specific_heat_J_kgK, surface_area
    )
    envelope.steady_state_rise_C = compute_steady_state_rise_C(
        envelope.total_heat_generation_W, surface_area
    )

    return envelope
