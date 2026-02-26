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


# ---------------------------------------------------------------------------
# Radiation Heat Loss (Stefan-Boltzmann)
# ---------------------------------------------------------------------------

class RadiationHeatModel:
    """Stefan-Boltzmann radiation heat transfer.

    Q_rad = epsilon * sigma * A * (T_s^4 - T_amb^4)

    sigma = 5.67e-8 W/(m^2.K^4) (Stefan-Boltzmann constant).
    """

    STEFAN_BOLTZMANN = 5.67e-8  # W/(m^2.K^4)

    @staticmethod
    def radiation_heat_W(
        emissivity: float,
        surface_area_m2: float,
        surface_T_K: float,
        ambient_T_K: float,
    ) -> float:
        """Radiation heat loss [W]."""
        sigma = RadiationHeatModel.STEFAN_BOLTZMANN
        return emissivity * sigma * surface_area_m2 * (
            surface_T_K**4 - ambient_T_K**4
        )

    @staticmethod
    def linearized_h_rad_W_m2K(
        emissivity: float,
        surface_T_K: float,
        ambient_T_K: float,
    ) -> float:
        """Linearized radiation heat transfer coefficient [W/(m^2.K)].

        h_rad = epsilon * sigma * (T_s^2 + T_amb^2) * (T_s + T_amb)

        Useful for combining with convective h in total heat transfer.
        """
        sigma = RadiationHeatModel.STEFAN_BOLTZMANN
        return emissivity * sigma * (
            surface_T_K**2 + ambient_T_K**2
        ) * (surface_T_K + ambient_T_K)


# ---------------------------------------------------------------------------
# Transient Thermal PDE Solver
# ---------------------------------------------------------------------------

class TransientThermalSolver:
    """Lumped-parameter transient thermal model.

    dT/dt = [Q_in - h_total * A * (T - T_amb)] / (m * c_p)

    where h_total = h_conv + h_rad.
    """

    @staticmethod
    def forward_euler_step(
        T_current_C: float,
        Q_in_W: float,
        h_total_W_m2K: float,
        surface_area_m2: float,
        mass_kg: float,
        specific_heat_J_kgK: float,
        ambient_T_C: float,
        dt_s: float,
    ) -> float:
        """Single Forward Euler time step for temperature [C].

        T(t+dt) = T(t) + dt * [Q_in - h*A*(T - T_amb)] / (m*c_p)
        """
        if mass_kg <= 0 or specific_heat_J_kgK <= 0:
            return T_current_C
        dTdt = (
            Q_in_W - h_total_W_m2K * surface_area_m2 * (T_current_C - ambient_T_C)
        ) / (mass_kg * specific_heat_J_kgK)
        return T_current_C + dt_s * dTdt

    @staticmethod
    def crank_nicolson_step(
        T_current_C: float,
        Q_in_W: float,
        h_total_W_m2K: float,
        surface_area_m2: float,
        mass_kg: float,
        specific_heat_J_kgK: float,
        ambient_T_C: float,
        dt_s: float,
    ) -> float:
        """Single Crank-Nicolson (implicit trapezoidal) time step [C].

        Unconditionally stable implicit averaging:
        T^{n+1} = [T^n*(1 - alpha*dt/2) + beta*dt] / (1 + alpha*dt/2)

        where alpha = h*A/(m*c_p), beta = (Q_in + h*A*T_amb)/(m*c_p).
        """
        if mass_kg <= 0 or specific_heat_J_kgK <= 0:
            return T_current_C
        mc = mass_kg * specific_heat_J_kgK
        hA = h_total_W_m2K * surface_area_m2
        alpha = hA / mc
        beta = (Q_in_W + hA * ambient_T_C) / mc
        numerator = T_current_C * (1.0 - alpha * dt_s / 2.0) + beta * dt_s
        denominator = 1.0 + alpha * dt_s / 2.0
        if denominator == 0:
            return T_current_C
        return numerator / denominator

    @staticmethod
    def warmup_curve(
        Q_in_W: float,
        h_total_W_m2K: float,
        surface_area_m2: float,
        mass_kg: float,
        specific_heat_J_kgK: float,
        ambient_T_C: float,
        duration_s: float,
        dt_s: float = 1.0,
        method: str = "crank_nicolson",
    ) -> List[Tuple[float, float]]:
        """Compute temperature vs time from cold start.

        Returns list of (time_s, temperature_C) tuples.
        """
        T = ambient_T_C
        history = [(0.0, T)]
        t = 0.0
        step_fn = (
            TransientThermalSolver.crank_nicolson_step
            if method == "crank_nicolson"
            else TransientThermalSolver.forward_euler_step
        )
        while t < duration_s:
            T = step_fn(
                T, Q_in_W, h_total_W_m2K, surface_area_m2,
                mass_kg, specific_heat_J_kgK, ambient_T_C, dt_s,
            )
            t += dt_s
            history.append((t, T))
        return history

    @staticmethod
    def steady_state_T_C(
        Q_in_W: float,
        h_total_W_m2K: float,
        surface_area_m2: float,
        ambient_T_C: float,
    ) -> float:
        """Analytical steady-state temperature [C].

        T_ss = T_amb + Q_in / (h * A)
        """
        hA = h_total_W_m2K * surface_area_m2
        if hA <= 0:
            return float("inf")
        return ambient_T_C + Q_in_W / hA

    @staticmethod
    def time_constant_s(
        mass_kg: float,
        specific_heat_J_kgK: float,
        h_total_W_m2K: float,
        surface_area_m2: float,
    ) -> float:
        """Thermal time constant tau = m*c_p / (h*A) [s]."""
        hA = h_total_W_m2K * surface_area_m2
        if hA <= 0:
            return float("inf")
        return mass_kg * specific_heat_J_kgK / hA


# ---------------------------------------------------------------------------
# Thermal-Clearance Feedback
# ---------------------------------------------------------------------------

class ThermalClearanceFeedback:
    """Combined thermal + wear clearance evolution."""

    @staticmethod
    def thermal_clearance_mm(
        alpha_bushing_per_K: float,
        alpha_shaft_per_K: float,
        bore_diameter_mm: float,
        T_current_C: float,
        T_ref_C: float = 20.0,
    ) -> float:
        """Clearance change from differential thermal expansion [mm].

        c(T) = (alpha_bushing - alpha_shaft) * d * (T - T_ref)
        """
        return (alpha_bushing_per_K - alpha_shaft_per_K) * bore_diameter_mm * (
            T_current_C - T_ref_C
        )

    @staticmethod
    def combined_clearance_mm(
        initial_clearance_mm: float,
        thermal_delta_mm: float,
        wear_delta_mm: float,
    ) -> float:
        """Total clearance combining initial, thermal, and wear contributions.

        c(t) = c_init + delta_c_thermal + delta_c_wear
        """
        return initial_clearance_mm + thermal_delta_mm + wear_delta_mm

    @staticmethod
    def is_seized(clearance_mm: float) -> bool:
        """Check if clearance has closed to seizure (c <= 0)."""
        return clearance_mm <= 0.0
