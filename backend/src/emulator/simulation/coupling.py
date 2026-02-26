"""
Inter-Module Coupling Functions for Analytical Engine Simulation

Implements dynamic coupling between physics modules:
- Thermal-tribology: viscosity(T) -> film thickness -> wear rate -> heat
- Kinematics-structural: bearing loads -> deflection -> load redistribution
- Tribology-structural: clearance -> bearing stiffness -> dynamic loads
"""

from __future__ import annotations

import math

from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.simulation.state import SimulationConfig, SimulationState
from backend.src.emulator.structural import ShaftAnalysis
from backend.src.emulator.thermodynamics import (
    FrictionHeatModel,
    RadiationHeatModel,
    ThermalClearanceFeedback,
    TransientThermalSolver,
)
from backend.src.emulator.tribology import (
    RunningInWear,
    WearModel,
)


class CouplingFunctions:
    """Static methods implementing cross-module coupling."""

    # -- Thermal-Tribology Coupling --

    @staticmethod
    def viscosity_at_temperature(
        eta_40_Pa_s: float,
        temperature_C: float,
        beta: float = 0.02,
    ) -> float:
        """Temperature-dependent oil viscosity.

        eta(T) = eta_40 * exp(-beta * (T - 40))

        Typical mineral oil: beta ~ 0.02 /C.
        """
        return eta_40_Pa_s * math.exp(-beta * (temperature_C - 40.0))

    @staticmethod
    def friction_heat_from_bearings(
        bearing_loads_N: list[float],
        shaft_diameter_mm: float,
        omega_rad_s: float,
        friction_coeff: float,
    ) -> float:
        """Total bearing friction heat [W]."""
        total = 0.0
        for load in bearing_loads_N:
            total += FrictionHeatModel.bearing_heat_W(
                friction_coeff, load, shaft_diameter_mm, omega_rad_s,
            )
        return total

    @staticmethod
    def thermal_step(
        state: SimulationState,
        config: SimulationConfig,
        Q_total_W: float,
    ) -> float:
        """Advance temperature by one time step. Returns new temperature [C].

        Uses Crank-Nicolson for unconditional stability.
        """
        # Include radiation via linearized h_rad
        T_s_K = state.temperature_C + 273.15
        T_amb_K = config.ambient_temperature_C + 273.15
        h_rad = RadiationHeatModel.linearized_h_rad_W_m2K(
            config.emissivity, T_s_K, T_amb_K,
        )
        h_total = config.h_convection_W_m2K + h_rad

        return TransientThermalSolver.crank_nicolson_step(
            state.temperature_C,
            Q_total_W,
            h_total,
            config.surface_area_m2,
            config.machine_mass_kg,
            460.0,  # c_p for steel/cast iron frame [J/(kg.K)]
            config.ambient_temperature_C,
            config.dt_s,
        )

    # -- Tribology-Structural Coupling --

    @staticmethod
    def bearing_wear_step(
        state: SimulationState,
        config: SimulationConfig,
        lib: MaterialLibrary,
    ) -> list[float]:
        """Compute bearing wear increments for one time step [mm^3].

        Returns list of wear volume increments per bearing.
        """
        bearing_mat = lib.get(config.bearing_material)
        H_MPa = WearModel.hardness_HB_to_MPa(bearing_mat.hardness_HB[0])

        # Sliding distance this time step
        ds_mm = (
            math.pi * config.shaft_diameter_mm * config.rpm / 60.0
        ) * config.dt_s

        increments = []
        for i, load in enumerate(state.bearing_loads_N):
            # Use running-in K if applicable
            K = RunningInWear.wear_coefficient(
                state.total_sliding_distance_mm,
                config.running_in_K_initial,
                config.archard_K_bearing,
                config.running_in_s0_mm,
            )
            dV = WearModel.archard_volume_mm3(K, load, ds_mm, H_MPa)
            increments.append(dV)
        return increments

    @staticmethod
    def update_bearing_clearances(
        state: SimulationState,
        config: SimulationConfig,
        lib: MaterialLibrary,
    ) -> list[float]:
        """Compute current bearing clearances combining thermal + wear [mm]."""
        bearing_mat = lib.get(config.bearing_material)
        shaft_mat = lib.get(config.shaft_material)

        clearances = []
        for i in range(config.bearing_count):
            # Thermal clearance change
            delta_thermal = ThermalClearanceFeedback.thermal_clearance_mm(
                bearing_mat.thermal_expansion_coeff_per_K,
                shaft_mat.thermal_expansion_coeff_per_K,
                config.shaft_diameter_mm,
                state.temperature_C,
            )
            # Wear clearance change
            wear_vol = state.bearing_wear_volumes_mm3[i] if i < len(
                state.bearing_wear_volumes_mm3
            ) else 0.0
            delta_wear_mm = wear_vol / (
                math.pi * config.shaft_diameter_mm * config.bearing_length_mm
            ) if config.bearing_length_mm > 0 else 0.0

            c = ThermalClearanceFeedback.combined_clearance_mm(
                config.initial_clearance_mm, delta_thermal, delta_wear_mm,
            )
            clearances.append(c)
        return clearances

    # -- Kinematics-Structural Coupling --

    @staticmethod
    def shaft_deflection_mm(
        state: SimulationState,
        config: SimulationConfig,
        lib: MaterialLibrary,
    ) -> float:
        """Current shaft deflection from bearing loads [mm]."""
        shaft_mat = lib.get(config.shaft_material)
        total_load = sum(state.bearing_loads_N)
        return ShaftAnalysis.max_deflection_multi_support_mm(
            total_load,
            config.shaft_length_mm,
            config.bearing_count,
            shaft_mat.youngs_modulus_GPa[0],
            config.shaft_diameter_mm,
        )

    @staticmethod
    def redistribute_bearing_loads(
        total_load_N: float,
        bearing_count: int,
        clearances_mm: list[float],
    ) -> list[float]:
        """Redistribute bearing loads based on clearance (stiffer = more load).

        Bearings with smaller clearance carry proportionally more load.
        Uses inverse-clearance weighting for load distribution.
        """
        if not clearances_mm or bearing_count <= 0:
            return []

        # Inverse clearance as stiffness proxy
        inv_c = []
        for c in clearances_mm:
            if c > 0:
                inv_c.append(1.0 / c)
            else:
                inv_c.append(1e6)  # Very stiff (near seizure)

        total_inv = sum(inv_c)
        if total_inv <= 0:
            return [total_load_N / bearing_count] * bearing_count

        return [total_load_N * ic / total_inv for ic in inv_c]
