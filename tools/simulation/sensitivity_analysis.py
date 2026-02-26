#!/usr/bin/env python3
"""Parameter sensitivity analysis for Babbage Engine physics model.

Performs one-at-a-time (OAT) +/-10% perturbation on key parameters and
ranks by normalized sensitivity coefficient:

    S = (dY/Y) / (dX/X)

where X is the input parameter and Y is the output metric.

Output metrics evaluated:
- Lewis bending stress [Pa]
- Bearing heat generation [W]
- Shaft deflection [m]
- Thermal expansion clearance change [m]
- Archard wear volume [mm^3]
- Eddy current loss [W]
- Euler buckling critical load [N]

Exit code 0: analysis complete, results printed.
"""

from __future__ import annotations

import math
import sys
from dataclasses import dataclass
from typing import Callable, List, Tuple


# ---------------------------------------------------------------------------
# Baseline parameter values (from sim_schema.yaml)
# ---------------------------------------------------------------------------

PARAMS = {
    # Gear parameters
    "tooth_count_driver":   20,
    "tooth_count_driven":   60,
    "module_mm":            2.5,
    "pressure_angle_deg":   20.0,
    "face_width_mm":        15.0,
    "gear_youngs_GPa":      97.5,      # brass midpoint
    "gear_yield_MPa":       285.0,     # brass midpoint

    # Shaft parameters
    "shaft_diameter_mm":    50.0,
    "shaft_span_mm":        375.0,     # bearing spacing
    "shaft_youngs_GPa":     205.0,     # steel midpoint
    "shaft_load_N":         500.0,     # estimated radial

    # Bearing parameters
    "bearing_friction":     0.15,      # brass
    "bearing_load_N":       125.0,     # 500N / 4 bearings
    "shaft_rpm":            30.0,

    # Thermal parameters
    "brass_alpha_per_K":    20.5e-6,
    "steel_alpha_per_K":    11.7e-6,
    "component_length_mm":  100.0,
    "delta_T_K":            20.0,      # 20C to 40C

    # Wear parameters
    "archard_K":            5e-6,      # brass-on-steel lubricated
    "normal_force_N":       50.0,
    "sliding_distance_mm":  1000.0,
    "hardness_HB":          120.0,     # brass

    # Electromagnetic
    "B_field_T":            50e-6,     # Earth's field
    "eddy_thickness_m":     0.050,     # shaft diameter
    "eddy_volume_m3":       0.0002,    # shaft volume
    "resistivity_ohm_m":    1.59e-7,   # steel

    # Structural
    "column_height_mm":     600.0,
    "column_width_mm":      25.0,
    "column_youngs_GPa":    205.0,     # steel
}


# ---------------------------------------------------------------------------
# Physics equations (return single scalar output)
# ---------------------------------------------------------------------------

def lewis_stress(p: dict) -> float:
    """Lewis bending stress: sigma = W_t / (b * m * Y) [MPa].

    Tangential load approximated from torque at pitch radius.
    """
    d_pitch = p["module_mm"] * p["tooth_count_driver"]  # mm
    # Torque from 500W at rpm: T = P/(2*pi*n/60)
    omega = 2 * math.pi * p["shaft_rpm"] / 60  # rad/s for driver
    # After gear reduction, shaft torque ~500W/omega
    T_Nm = 500.0 / max(omega, 0.01)
    W_t = T_Nm / (d_pitch / 2000.0)  # N
    Y = 0.32  # Lewis form factor for 20 teeth
    sigma = W_t / (p["face_width_mm"] * p["module_mm"] * Y)  # N/mm^2 = MPa
    return sigma


def bearing_heat(p: dict) -> float:
    """Bearing friction heat: Q = 0.5 * mu * W * d * omega [W]."""
    omega = 2 * math.pi * p["shaft_rpm"] / 60
    d_m = p["shaft_diameter_mm"] / 1000.0
    return 0.5 * p["bearing_friction"] * p["bearing_load_N"] * d_m * omega


def shaft_deflection(p: dict) -> float:
    """Simply-supported beam midspan deflection: delta = F*L^3/(48*E*I) [mm]."""
    L_m = p["shaft_span_mm"] / 1000.0
    E_Pa = p["shaft_youngs_GPa"] * 1e9
    d_m = p["shaft_diameter_mm"] / 1000.0
    I = math.pi * d_m**4 / 64.0
    delta_m = p["shaft_load_N"] * L_m**3 / (48 * E_Pa * I)
    return delta_m * 1000.0  # mm


def thermal_clearance_change(p: dict) -> float:
    """Dissimilar-metal clearance change [um]."""
    L_m = p["component_length_mm"] / 1000.0
    delta_brass = p["brass_alpha_per_K"] * L_m * p["delta_T_K"]
    delta_steel = p["steel_alpha_per_K"] * L_m * p["delta_T_K"]
    return abs(delta_brass - delta_steel) * 1e6  # um


def archard_wear(p: dict) -> float:
    """Archard wear volume: V = K * F * s / H [mm^3]."""
    H_MPa = p["hardness_HB"] * 9.81  # approximate Brinell to MPa
    return p["archard_K"] * p["normal_force_N"] * p["sliding_distance_mm"] / H_MPa


def eddy_loss(p: dict) -> float:
    """Eddy current loss: P = pi^2*B^2*d^2*f^2*V/(6*rho) [W]."""
    f = p["shaft_rpm"] / 60.0  # Hz
    d = p["eddy_thickness_m"]
    B = p["B_field_T"]
    V = p["eddy_volume_m3"]
    rho = p["resistivity_ohm_m"]
    return (math.pi**2 * B**2 * d**2 * f**2 * V) / (6 * rho)


def euler_buckling(p: dict) -> float:
    """Euler critical buckling load: P_cr = pi^2*E*I/(K*L)^2 [N].

    Fixed-fixed end condition: K=0.5.
    """
    E_Pa = p["column_youngs_GPa"] * 1e9
    w_m = p["column_width_mm"] / 1000.0
    I = w_m**4 / 12.0  # square cross section
    K = 0.5  # fixed-fixed
    L_m = p["column_height_mm"] / 1000.0
    return math.pi**2 * E_Pa * I / (K * L_m)**2


# ---------------------------------------------------------------------------
# Sensitivity calculation
# ---------------------------------------------------------------------------

@dataclass
class SensitivityResult:
    param_name: str
    output_name: str
    baseline_output: float
    sensitivity_coeff: float


def compute_sensitivity(
    param_name: str,
    output_fn: Callable,
    output_name: str,
    perturbation: float = 0.10,
) -> SensitivityResult:
    """Compute normalized sensitivity coefficient for one parameter-output pair."""
    base_params = dict(PARAMS)
    baseline = output_fn(base_params)

    # +10% perturbation
    p_plus = dict(PARAMS)
    p_plus[param_name] = PARAMS[param_name] * (1 + perturbation)
    y_plus = output_fn(p_plus)

    # -10% perturbation
    p_minus = dict(PARAMS)
    p_minus[param_name] = PARAMS[param_name] * (1 - perturbation)
    y_minus = output_fn(p_minus)

    # Central difference sensitivity
    dY = (y_plus - y_minus) / 2.0
    dX = PARAMS[param_name] * perturbation

    if abs(baseline) < 1e-30:
        S = 0.0
    else:
        S = (dY / baseline) / (dX / PARAMS[param_name]) if dX != 0 else 0.0

    return SensitivityResult(
        param_name=param_name,
        output_name=output_name,
        baseline_output=baseline,
        sensitivity_coeff=S,
    )


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

OUTPUT_FUNCTIONS = [
    ("Lewis stress [MPa]", lewis_stress, [
        "tooth_count_driver", "module_mm", "face_width_mm", "shaft_rpm",
    ]),
    ("Bearing heat [W]", bearing_heat, [
        "bearing_friction", "bearing_load_N", "shaft_diameter_mm", "shaft_rpm",
    ]),
    ("Shaft deflection [mm]", shaft_deflection, [
        "shaft_load_N", "shaft_span_mm", "shaft_youngs_GPa", "shaft_diameter_mm",
    ]),
    ("Thermal clearance change [um]", thermal_clearance_change, [
        "brass_alpha_per_K", "steel_alpha_per_K", "component_length_mm", "delta_T_K",
    ]),
    ("Archard wear [mm^3]", archard_wear, [
        "archard_K", "normal_force_N", "sliding_distance_mm", "hardness_HB",
    ]),
    ("Eddy current loss [W]", eddy_loss, [
        "B_field_T", "eddy_thickness_m", "shaft_rpm", "resistivity_ohm_m",
    ]),
    ("Euler buckling [N]", euler_buckling, [
        "column_youngs_GPa", "column_width_mm", "column_height_mm",
    ]),
]


def main() -> int:
    all_results: List[SensitivityResult] = []

    for output_name, output_fn, param_names in OUTPUT_FUNCTIONS:
        print(f"\n--- {output_name} ---")
        baseline = output_fn(dict(PARAMS))
        print(f"  Baseline value: {baseline:.6g}")

        for pname in param_names:
            result = compute_sensitivity(pname, output_fn, output_name)
            all_results.append(result)
            print(f"  {pname:30s}  S = {result.sensitivity_coeff:+.4f}")

    # Rank all by absolute sensitivity
    all_results.sort(key=lambda r: abs(r.sensitivity_coeff), reverse=True)

    print("\n\n=== TOP 10 PARAMETERS BY SENSITIVITY ===")
    print(f"{'Rank':<6}{'Parameter':<30}{'Output':<35}{'|S|':<10}")
    print("-" * 81)
    for i, r in enumerate(all_results[:10], 1):
        print(f"{i:<6}{r.param_name:<30}{r.output_name:<35}{abs(r.sensitivity_coeff):<10.4f}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
