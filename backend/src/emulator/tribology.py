"""
Tribology and Wear Model for Babbage Analytical Engine

Models bearing life, gear wear, surface finish requirements, and lubrication
film thickness for all wearing surfaces.

Key equations:
- Archard wear: V = K * F_N * s / H  (Shigley Ch.12)
- PV limit (plain bearing): P*V < PV_limit  (~10 MPa.m/s for lubricated bronze-on-steel)
- Hamrock-Dowson EHL: h_c = 2.65 * R * U^0.7 * G^0.54 * W'^(-0.13)
- Lambda ratio: lambda = h_min / sqrt(Ra_1^2 + Ra_2^2); lambda > 3 for full-film
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Archard wear coefficients (dimensionless, Shigley Table 12-1)
# K values for lubricated contacts
ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL = 1e-6
ARCHARD_K_LUBRICATED_BRASS_ON_STEEL = 5e-6
ARCHARD_K_DRY_STEEL_ON_STEEL = 5e-4

# PV limits [MPa.m/s]
PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED = 10.0
PV_LIMIT_BRASS_ON_STEEL_LUBRICATED = 5.0


# ---------------------------------------------------------------------------
# Archard Wear Model
# ---------------------------------------------------------------------------

class WearModel:
    """Archard adhesive wear equation: V = K * F_N * s / H.

    V: wear volume [mm^3]
    K: dimensionless wear coefficient
    F_N: normal force [N]
    s: sliding distance [mm]
    H: hardness of softer material [MPa] (HB * 9.81 for Brinell to MPa)
    """

    @staticmethod
    def archard_volume_mm3(
        K: float,
        normal_force_N: float,
        sliding_distance_mm: float,
        hardness_MPa: float,
    ) -> float:
        """Archard wear volume [mm^3]."""
        if hardness_MPa <= 0:
            return float("inf")
        return K * normal_force_N * sliding_distance_mm / hardness_MPa

    @staticmethod
    def hardness_HB_to_MPa(HB: float) -> float:
        """Convert Brinell hardness to approximate MPa (HB * 9.81)."""
        return HB * 9.81

    @staticmethod
    def sliding_distance_per_hour_mm(
        diameter_mm: float,
        rpm: float,
    ) -> float:
        """Sliding distance for a rotating shaft in bearing [mm/hour].

        s = pi * d * n * 60
        """
        return math.pi * diameter_mm * rpm * 60.0

    @staticmethod
    def bearing_wear_volume_per_hour_mm3(
        K: float,
        radial_load_N: float,
        shaft_diameter_mm: float,
        rpm: float,
        hardness_HB: float,
    ) -> float:
        """Bearing wear volume per hour of operation [mm^3/hr]."""
        s_per_hr = WearModel.sliding_distance_per_hour_mm(shaft_diameter_mm, rpm)
        h_mpa = WearModel.hardness_HB_to_MPa(hardness_HB)
        return WearModel.archard_volume_mm3(K, radial_load_N, s_per_hr, h_mpa)

    @staticmethod
    def gear_wear_volume_per_hour_mm3(
        K: float,
        tangential_force_N: float,
        pitch_diameter_mm: float,
        rpm: float,
        tooth_count: int,
        hardness_HB: float,
    ) -> float:
        """Gear tooth wear per hour [mm^3/hr].

        Each tooth engages once per revolution. Approximate sliding distance
        per mesh: ~2 * addendum * sin(pressure_angle).
        """
        # Simplified: sliding per mesh ~ 0.5 mm (typical for module 2.5)
        slide_per_mesh_mm = 0.5
        meshes_per_hour = rpm * 60.0
        total_sliding = slide_per_mesh_mm * meshes_per_hour
        h_mpa = WearModel.hardness_HB_to_MPa(hardness_HB)
        return WearModel.archard_volume_mm3(K, tangential_force_N, total_sliding, h_mpa)


# ---------------------------------------------------------------------------
# PV Limit Analysis
# ---------------------------------------------------------------------------

class PVAnalysis:
    """Pressure-velocity (PV) product analysis for plain bearings.

    PV = (F / (d*L)) * (pi*d*n/60000)

    where:
    F: radial load [N]
    d: shaft diameter [mm]
    L: bearing length [mm]
    n: RPM
    """

    @staticmethod
    def bearing_pressure_MPa(
        radial_load_N: float,
        shaft_diameter_mm: float,
        bearing_length_mm: float,
    ) -> float:
        """Projected bearing pressure P = F / (d * L) [MPa].

        d and L in mm, F in N -> result in N/mm^2 = MPa.
        """
        projected_area = shaft_diameter_mm * bearing_length_mm
        if projected_area <= 0:
            return float("inf")
        return radial_load_N / projected_area

    @staticmethod
    def surface_velocity_m_s(shaft_diameter_mm: float, rpm: float) -> float:
        """Surface velocity V = pi * d * n / 60000 [m/s]."""
        return math.pi * shaft_diameter_mm * rpm / 60000.0

    @staticmethod
    def pv_product_MPa_m_s(
        radial_load_N: float,
        shaft_diameter_mm: float,
        bearing_length_mm: float,
        rpm: float,
    ) -> float:
        """PV product [MPa.m/s]."""
        p = PVAnalysis.bearing_pressure_MPa(
            radial_load_N, shaft_diameter_mm, bearing_length_mm
        )
        v = PVAnalysis.surface_velocity_m_s(shaft_diameter_mm, rpm)
        return p * v

    @staticmethod
    def is_within_limit(
        pv: float,
        pv_limit: float = PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED,
    ) -> bool:
        """Check if PV product is below allowable limit."""
        return pv < pv_limit


# ---------------------------------------------------------------------------
# Lubrication Film Thickness (Hamrock-Dowson EHL)
# ---------------------------------------------------------------------------

class LubricationModel:
    """Elastohydrodynamic lubrication (EHL) film thickness model.

    Hamrock-Dowson central film thickness:
    h_c = 2.65 * R * U^0.7 * G^0.54 * W'^(-0.13)

    U = eta_0 * (u1+u2) / (2*E'*R)  -- speed parameter
    G = alpha * E'                    -- materials parameter
    W' = W / (E'*R^2)                -- load parameter

    For journal bearings, use Petroff's equation instead:
    h_min ~ c * (1 - epsilon), where epsilon is eccentricity ratio.
    """

    @staticmethod
    def minimum_film_thickness_um(
        viscosity_Pa_s: float,
        entrainment_velocity_m_s: float,
        effective_radius_mm: float,
        reduced_modulus_GPa: float,
        load_per_length_N_mm: float,
        pressure_viscosity_coeff_per_GPa: float = 20.0,
    ) -> float:
        """Hamrock-Dowson line-contact minimum film thickness [um].

        Simplified Dowson-Higginson for line contacts:
        h_min = 1.6 * (eta*u)^0.7 * R^0.43 * (alpha*E')^0.54 / (E'*W')^0.13

        For initial estimates, we use a dimensional approach.
        """
        R_m = effective_radius_mm / 1000.0
        E_prime_Pa = reduced_modulus_GPa * 1e9
        eta = viscosity_Pa_s
        u = entrainment_velocity_m_s
        alpha = pressure_viscosity_coeff_per_GPa * 1e-9  # [1/Pa]
        w_prime = (load_per_length_N_mm * 1000.0) / (E_prime_Pa * R_m)

        U_param = eta * u / (E_prime_Pa * R_m)
        G_param = alpha * E_prime_Pa

        # Dowson-Higginson line contact:
        # H_min = 1.6 * U^0.7 * G^0.54 * W'^(-0.13)
        H_min = 1.6 * U_param**0.7 * G_param**0.54 * w_prime**(-0.13)
        h_min_m = H_min * R_m
        return h_min_m * 1e6  # convert to um

    @staticmethod
    def lambda_ratio(
        h_min_um: float,
        Ra_1_um: float,
        Ra_2_um: float,
    ) -> float:
        """Lambda ratio: lambda = h_min / sqrt(Ra_1^2 + Ra_2^2).

        lambda > 3: full-film (hydrodynamic)
        1 < lambda < 3: mixed lubrication
        lambda < 1: boundary lubrication
        """
        rms = math.sqrt(Ra_1_um**2 + Ra_2_um**2)
        if rms <= 0:
            return float("inf")
        return h_min_um / rms

    @staticmethod
    def regime(lam: float) -> str:
        """Classify lubrication regime from lambda ratio."""
        if lam >= 3.0:
            return "full_film"
        elif lam >= 1.0:
            return "mixed"
        else:
            return "boundary"


# ---------------------------------------------------------------------------
# Surface Finish Specification
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class SurfaceFinishSpec:
    """Surface finish requirements for a wearing surface."""
    component: str
    Ra_um: float
    process: str
    notes: str = ""


# 19th-century achievable finishes
PERIOD_SURFACE_FINISHES = [
    SurfaceFinishSpec("shaft_journal", 0.8, "turning + polishing",
                      "Best achievable with 19th-c lathe + hand polish"),
    SurfaceFinishSpec("gear_tooth_flank", 1.6, "hobbing + hand filing",
                      "Typical for brass gear teeth"),
    SurfaceFinishSpec("bearing_bore", 0.8, "boring + burnishing",
                      "Bronze bushing ID finish"),
    SurfaceFinishSpec("cam_surface", 1.6, "milling + polishing",
                      "Steel cam profile surface"),
]
