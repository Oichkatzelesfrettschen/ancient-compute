"""
Simulation State and Configuration for Analytical Engine Physics

SimulationState holds the complete state vector at a point in simulated time:
temperature, clearances, wear volumes, shaft angle, forces, and timestamp.

SimulationConfig holds all parameters needed to initialize and run a simulation.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple


@dataclass
class SimulationState:
    """Complete simulation state vector at a point in time.

    All quantities are in SI-compatible units (mm, N, C, rad/s, etc.)
    matching the rest of the physics modules.
    """

    # Time
    time_s: float = 0.0
    shaft_angle_deg: float = 0.0
    rotation_count: int = 0

    # Thermal state
    temperature_C: float = 20.0
    ambient_temperature_C: float = 20.0
    total_heat_generation_W: float = 0.0
    radiation_heat_loss_W: float = 0.0

    # Bearing state (per-bearing, indexed by position)
    bearing_clearances_mm: List[float] = field(default_factory=list)
    bearing_wear_volumes_mm3: List[float] = field(default_factory=list)
    bearing_loads_N: List[float] = field(default_factory=list)

    # Gear state
    gear_backlash_mm: float = 0.0
    gear_wear_volume_mm3: float = 0.0
    gear_contact_stress_MPa: float = 0.0
    gear_bending_stress_MPa: float = 0.0

    # Lubrication
    oil_viscosity_Pa_s: float = 0.1
    lambda_ratio: float = 3.0
    lubrication_regime: str = "full_film"

    # Structural
    shaft_deflection_mm: float = 0.0
    critical_speed_margin: float = 10.0

    # Forces at current shaft angle
    cam_torque_Nm: float = 0.0
    total_torque_Nm: float = 0.0
    energy_consumed_J: float = 0.0

    # accumulated sliding distance for running-in model
    total_sliding_distance_mm: float = 0.0
    galvanic_risk_accumulator: float = 0.0

    def copy(self) -> SimulationState:
        """Return a deep copy of this state."""
        return SimulationState(
            time_s=self.time_s,
            shaft_angle_deg=self.shaft_angle_deg,
            rotation_count=self.rotation_count,
            temperature_C=self.temperature_C,
            ambient_temperature_C=self.ambient_temperature_C,
            total_heat_generation_W=self.total_heat_generation_W,
            radiation_heat_loss_W=self.radiation_heat_loss_W,
            bearing_clearances_mm=list(self.bearing_clearances_mm),
            bearing_wear_volumes_mm3=list(self.bearing_wear_volumes_mm3),
            bearing_loads_N=list(self.bearing_loads_N),
            gear_backlash_mm=self.gear_backlash_mm,
            gear_wear_volume_mm3=self.gear_wear_volume_mm3,
            gear_contact_stress_MPa=self.gear_contact_stress_MPa,
            gear_bending_stress_MPa=self.gear_bending_stress_MPa,
            oil_viscosity_Pa_s=self.oil_viscosity_Pa_s,
            lambda_ratio=self.lambda_ratio,
            lubrication_regime=self.lubrication_regime,
            shaft_deflection_mm=self.shaft_deflection_mm,
            critical_speed_margin=self.critical_speed_margin,
            cam_torque_Nm=self.cam_torque_Nm,
            total_torque_Nm=self.total_torque_Nm,
            total_sliding_distance_mm=self.total_sliding_distance_mm,
            galvanic_risk_accumulator=self.galvanic_risk_accumulator,
        )


@dataclass
class SimulationConfig:
    """Configuration parameters for a simulation run."""

    # Time stepping
    dt_s: float = 1.0             # Time step [s]
    rpm: float = 30.0             # Operating speed [RPM]

    # Machine geometry
    shaft_diameter_mm: float = 50.0
    shaft_length_mm: float = 1500.0
    bearing_count: int = 4
    bearing_length_mm: float = 60.0

    # Initial conditions
    initial_clearance_mm: float = 0.05
    initial_gear_backlash_mm: float = 0.02
    ambient_temperature_C: float = 20.0

    # Material names (resolved from MaterialLibrary)
    shaft_material: str = "steel"
    bearing_material: str = "phosphor_bronze"
    gear_material: str = "brass"

    # Thermal
    machine_mass_kg: float = 500.0
    surface_area_m2: float = 7.2
    h_convection_W_m2K: float = 10.0
    emissivity: float = 0.8

    # Wear
    archard_K_bearing: float = 1e-6
    archard_K_gear: float = 5e-6
    running_in_K_initial: float = 1e-5
    running_in_s0_mm: float = 1e7

    # Gear parameters
    gear_tooth_count: int = 20
    gear_module_mm: float = 2.5
    gear_face_width_mm: float = 15.0
    transmitted_power_W: float = 50.0

    # Failure limits
    clearance_limit_mm: float = 0.15
    backlash_limit_mm: float = 0.10
    temperature_limit_C: float = 60.0

    # Valve gear (Stephenson link) -- provisional values
    valve_lap_mm: float = 3.0
    valve_lead_mm: float = 1.0
    valve_cutoff_pct: float = 60.0

    # Steam drive -- provisional values
    steam_pressure_bar: float = 6.0
    piston_stroke_m: float = 0.2
    thermal_efficiency_pct: float = 8.0

    def randomize_tolerances(self, seed: Optional[int] = None) -> None:
        """Inject Monte Carlo variance based on historical manufacturing precision.
        
        Babbage Era (1840s) achievable precision was ~0.002 inches (0.05 mm).
        This method applies a Gaussian perturbation to key geometric parameters.
        """
        import random
        if seed is not None:
            random.seed(seed)
            
        # 0.05mm is the 3-sigma target based on historical sources (Doron Swade)
        sigma = 0.05 / 3.0
        
        self.initial_clearance_mm += random.gauss(0, sigma)
        self.initial_gear_backlash_mm += random.gauss(0, sigma)
        
        # Gear module also subject to manufacturing variance
        self.gear_module_mm += random.gauss(0, sigma / 10.0) # Module variance is typically smaller
        
        # Clamp to ensure physical sanity
        self.initial_clearance_mm = max(0.001, self.initial_clearance_mm)
        self.initial_gear_backlash_mm = max(0.001, self.initial_gear_backlash_mm)
        self.gear_module_mm = max(0.1, self.gear_module_mm)

    @property
    def omega_rad_s(self) -> float:
        """Angular velocity [rad/s]."""
        return 2.0 * math.pi * self.rpm / 60.0

    @property
    def period_s(self) -> float:
        """Period of one shaft rotation [s]."""
        if self.rpm <= 0:
            return float("inf")
        return 60.0 / self.rpm
