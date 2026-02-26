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
        eta_100_Pa_s: float | None = None,
        density_kg_m3: float = 870.0,
    ) -> float:
        """Temperature-dependent dynamic oil viscosity [Pa*s] via ASTM D341.

        WHY: The exponential approximation eta = eta_40 * exp(-beta*(T-40))
        is only valid over a narrow range (~20 deg) and underestimates
        viscosity at low temperatures and overestimates at high temperatures.
        ASTM D341 (Walther equation) is the industry standard for predicting
        kinematic viscosity of mineral oils over the full operating range.

        ASTM D341 / Walther equation in kinematic viscosity [cSt]:
            log10(log10(nu + 0.7)) = A - B * log10(T_K)
        where T_K = temperature + 273.15 (Kelvin), nu in cSt.

        A and B are determined from two reference points (40 C and 100 C).
        If eta_100_Pa_s is not provided, the equation degrades to a single-
        point exponential fallback using B = 4.0 (typical for ISO VG 68).

        Reference: ASTM D341-20, "Standard Practice for Viscosity-Temperature
        Charts for Liquid Petroleum Products", ASTM International.

        For ISO VG 68: nu_40 ~ 68 cSt, nu_100 ~ 8.7 cSt (approximate).
        density ~ 870 kg/m^3 at 15 C.

        Args:
            eta_40_Pa_s: Dynamic viscosity at 40 C [Pa*s]
            temperature_C: Operating temperature [C]
            eta_100_Pa_s: Dynamic viscosity at 100 C [Pa*s] (improves accuracy)
            density_kg_m3: Oil density [kg/m^3] for cSt -> Pa*s conversion

        Returns:
            Dynamic viscosity at temperature_C [Pa*s]
        """
        T_K = temperature_C + 273.15
        nu_40_cSt = (eta_40_Pa_s * 1e6) / density_kg_m3  # Pa*s -> cSt

        if eta_100_Pa_s is not None:
            nu_100_cSt = (eta_100_Pa_s * 1e6) / density_kg_m3
            # Solve for A and B from the two reference points
            T40_K = 313.15  # 40 C in K
            T100_K = 373.15  # 100 C in K
            W40 = math.log10(math.log10(nu_40_cSt + 0.7))
            W100 = math.log10(math.log10(nu_100_cSt + 0.7))
            B = (W40 - W100) / (math.log10(T100_K) - math.log10(T40_K))
            A = W40 + B * math.log10(T40_K)
        else:
            # Single-point: use typical B=3.5 for mineral oil (conservative)
            T40_K = 313.15
            W40 = math.log10(math.log10(nu_40_cSt + 0.7))
            B = 3.5
            A = W40 + B * math.log10(T40_K)

        W_T = A - B * math.log10(T_K)
        nu_T_cSt = max(0.5, 10.0 ** (10.0**W_T) - 0.7)
        eta_T_Pa_s = (nu_T_cSt * density_kg_m3) / 1e6
        return eta_T_Pa_s

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
