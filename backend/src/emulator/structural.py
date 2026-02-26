"""
Structural Analysis Module for Babbage Analytical Engine

Verifies structural integrity of major components:
- Shaft deflection (Euler-Bernoulli beam, Shigley Ch.4)
- Gear tooth bending stress (Lewis/AGMA, Shigley Ch.14)
- Fatigue life via Goodman diagram (Shigley Ch.6)
- Column buckling via Euler/Johnson (Shigley Ch.4)

Acceptance criteria:
- Safety factor >= 2.0 for all structural members
- Shaft deflection < L/10000
- Fatigue life > 10^8 cycles
- Buckling SF >= 3.0 for vertical supports
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Optional, Tuple


# ---------------------------------------------------------------------------
# Shaft Deflection Analysis (Euler-Bernoulli, Shigley Ch.4)
# ---------------------------------------------------------------------------

class ShaftAnalysis:
    """Simply-supported beam deflection for multi-support shafts."""

    @staticmethod
    def moment_of_inertia_m4(diameter_mm: float) -> float:
        """Second moment of area for solid circular shaft: I = pi*d^4/64."""
        d_m = diameter_mm / 1000.0
        return math.pi * d_m**4 / 64.0

    @staticmethod
    def max_deflection_simply_supported_mm(
        force_N: float,
        span_mm: float,
        youngs_modulus_GPa: float,
        diameter_mm: float,
    ) -> float:
        """Maximum midspan deflection for simply-supported beam with center load.

        delta_max = F * L^3 / (48 * E * I)  (Shigley Eq.4-10)
        """
        L_m = span_mm / 1000.0
        E_Pa = youngs_modulus_GPa * 1e9
        I = ShaftAnalysis.moment_of_inertia_m4(diameter_mm)
        if E_Pa <= 0 or I <= 0:
            return float("inf")
        delta_m = force_N * L_m**3 / (48.0 * E_Pa * I)
        return delta_m * 1000.0  # convert to mm

    @staticmethod
    def max_deflection_multi_support_mm(
        total_load_N: float,
        total_length_mm: float,
        bearing_count: int,
        youngs_modulus_GPa: float,
        diameter_mm: float,
    ) -> float:
        """Approximate deflection for multi-support shaft.

        Treats each span between bearings as independently supported.
        Effective span = total_length / (bearing_count - 1).
        Load per span = total_load / (bearing_count - 1).
        """
        if bearing_count < 2:
            return ShaftAnalysis.max_deflection_simply_supported_mm(
                total_load_N, total_length_mm, youngs_modulus_GPa, diameter_mm,
            )
        n_spans = bearing_count - 1
        span = total_length_mm / n_spans
        load_per_span = total_load_N / n_spans
        return ShaftAnalysis.max_deflection_simply_supported_mm(
            load_per_span, span, youngs_modulus_GPa, diameter_mm,
        )

    @staticmethod
    def deflection_limit_mm(span_mm: float, ratio: float = 10000.0) -> float:
        """Allowable deflection: L / ratio [mm]."""
        return span_mm / ratio


# ---------------------------------------------------------------------------
# Gear Tooth Stress (AGMA/Lewis, Shigley Ch.14)
# ---------------------------------------------------------------------------

class GearToothStress:
    """Gear tooth bending stress analysis with safety factor."""

    @staticmethod
    def lewis_form_factor(teeth: int) -> float:
        """Lewis form factor Y for 20-deg full-depth teeth."""
        if teeth < 12:
            return 0.245
        return 0.484 - (2.87 / teeth)

    @staticmethod
    def bending_stress_MPa(
        tangential_force_N: float,
        face_width_mm: float,
        module_mm: float,
        tooth_count: int,
    ) -> float:
        """Lewis bending stress sigma = W_t / (b * m * Y) [MPa]."""
        y = GearToothStress.lewis_form_factor(tooth_count)
        return tangential_force_N / (face_width_mm * module_mm * y)

    @staticmethod
    def safety_factor(
        yield_strength_MPa: float,
        applied_stress_MPa: float,
    ) -> float:
        """Safety factor = Sy / sigma_applied."""
        if applied_stress_MPa <= 0:
            return float("inf")
        return yield_strength_MPa / applied_stress_MPa


# ---------------------------------------------------------------------------
# Fatigue Analysis (Goodman Diagram, Shigley Ch.6)
# ---------------------------------------------------------------------------

class FatigueAnalysis:
    """Modified Goodman criterion for infinite-life fatigue design.

    sigma_a / Se + sigma_m / Su = 1/SF

    Endurance limit correction factors:
    Se = ka * kb * kc * kd * ke * Se'
    ka: surface factor
    kb: size factor
    kc: reliability factor
    kd: temperature factor
    ke: miscellaneous factor
    """

    @staticmethod
    def surface_factor_ka(Su_MPa: float, finish: str = "machined") -> float:
        """Marin surface factor ka (Shigley Eq.6-18).

        ka = a * Su^b where a,b from Table 6-2.
        """
        factors = {
            "ground": (1.58, -0.085),
            "machined": (4.51, -0.265),
            "hot_rolled": (57.7, -0.718),
            "forged": (272.0, -0.995),
        }
        a, b = factors.get(finish, factors["machined"])
        return a * Su_MPa**b

    @staticmethod
    def size_factor_kb(diameter_mm: float) -> float:
        """Marin size factor kb (Shigley Eq.6-19).

        For 2.79 <= d <= 51 mm: kb = 1.24 * d^(-0.107)
        For 51 < d <= 254 mm: kb = 1.51 * d^(-0.157)
        """
        if diameter_mm <= 2.79:
            return 1.0
        elif diameter_mm <= 51.0:
            return 1.24 * diameter_mm**(-0.107)
        elif diameter_mm <= 254.0:
            return 1.51 * diameter_mm**(-0.157)
        else:
            return 0.6

    @staticmethod
    def corrected_endurance_limit_MPa(
        Se_prime_MPa: float,
        Su_MPa: float,
        diameter_mm: float,
        finish: str = "machined",
        reliability_factor: float = 0.897,  # 90% reliability
        temperature_factor: float = 1.0,
    ) -> float:
        """Corrected endurance limit Se = ka*kb*kc*kd*Se' [MPa]."""
        ka = FatigueAnalysis.surface_factor_ka(Su_MPa, finish)
        kb = FatigueAnalysis.size_factor_kb(diameter_mm)
        kc = reliability_factor
        kd = temperature_factor
        return ka * kb * kc * kd * Se_prime_MPa

    @staticmethod
    def goodman_safety_factor(
        stress_amplitude_MPa: float,
        mean_stress_MPa: float,
        Se_MPa: float,
        Su_MPa: float,
    ) -> float:
        """Modified Goodman safety factor (Shigley Eq.6-41).

        1/SF = sigma_a/Se + sigma_m/Su
        """
        if Se_MPa <= 0 or Su_MPa <= 0:
            return 0.0
        inv_sf = stress_amplitude_MPa / Se_MPa + mean_stress_MPa / Su_MPa
        if inv_sf <= 0:
            return float("inf")
        return 1.0 / inv_sf

    @staticmethod
    def fatigue_life_cycles(
        stress_amplitude_MPa: float,
        Se_MPa: float,
        Su_MPa: float,
        exponent: float = -0.085,
    ) -> float:
        """Approximate fatigue life using Basquin's equation.

        If sigma_a <= Se: infinite life (return 1e9).
        If sigma_a > Su: return 0 (immediate failure).
        Otherwise: N = (sigma_a / a)^(1/b) where a,b fit the S-N curve.
        """
        if stress_amplitude_MPa <= Se_MPa:
            return 1e9  # Infinite life
        if stress_amplitude_MPa >= Su_MPa:
            return 0.0
        # Simplified S-N curve: log-linear between (1e3, 0.9*Su) and (1e6, Se)
        n_low, s_low = 1e3, 0.9 * Su_MPa
        n_high, s_high = 1e6, Se_MPa
        if s_low <= s_high or s_low <= 0 or s_high <= 0:
            return 1e6
        b = math.log10(s_low / s_high) / math.log10(n_high / n_low)
        a = s_low / (n_low ** (-b))
        if b == 0:
            return 1e6
        return (stress_amplitude_MPa / a) ** (1.0 / (-b))


# ---------------------------------------------------------------------------
# Buckling Analysis (Euler/Johnson, Shigley Ch.4)
# ---------------------------------------------------------------------------

class BucklingAnalysis:
    """Column buckling analysis for vertical supports."""

    @staticmethod
    def slenderness_ratio(
        length_mm: float,
        effective_length_factor: float,
        moment_of_inertia_mm4: float,
        cross_section_area_mm2: float,
    ) -> float:
        """Slenderness ratio lambda = K*L / r, where r = sqrt(I/A)."""
        if cross_section_area_mm2 <= 0 or moment_of_inertia_mm4 <= 0:
            return float("inf")
        r = math.sqrt(moment_of_inertia_mm4 / cross_section_area_mm2)
        return effective_length_factor * length_mm / r

    @staticmethod
    def euler_critical_load_N(
        youngs_modulus_GPa: float,
        moment_of_inertia_mm4: float,
        length_mm: float,
        effective_length_factor: float,
    ) -> float:
        """Euler critical buckling load: P_cr = pi^2*E*I / (K*L)^2 [N].

        E in GPa, I in mm^4, L in mm -> convert to consistent units.
        """
        E_MPa = youngs_modulus_GPa * 1000.0  # GPa to MPa (= N/mm^2)
        effective_length = effective_length_factor * length_mm
        if effective_length <= 0:
            return float("inf")
        return math.pi**2 * E_MPa * moment_of_inertia_mm4 / effective_length**2

    @staticmethod
    def rectangular_section_I_mm4(width_mm: float, height_mm: float) -> float:
        """Second moment of area for rectangle: I = b*h^3/12 [mm^4]."""
        return width_mm * height_mm**3 / 12.0

    @staticmethod
    def rectangular_section_area_mm2(width_mm: float, height_mm: float) -> float:
        return width_mm * height_mm

    @staticmethod
    def buckling_safety_factor(
        critical_load_N: float,
        applied_load_N: float,
    ) -> float:
        """Buckling safety factor = P_cr / P_applied."""
        if applied_load_N <= 0:
            return float("inf")
        return critical_load_N / applied_load_N
