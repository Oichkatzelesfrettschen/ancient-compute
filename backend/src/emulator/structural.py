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
        return float(a * Su_MPa**b)

    @staticmethod
    def size_factor_kb(diameter_mm: float) -> float:
        """Marin size factor kb (Shigley Eq.6-19).

        For 2.79 <= d <= 51 mm: kb = 1.24 * d^(-0.107)
        For 51 < d <= 254 mm: kb = 1.51 * d^(-0.157)
        """
        if diameter_mm <= 2.79:
            return 1.0
        elif diameter_mm <= 51.0:
            return float(1.24 * diameter_mm**(-0.107))
        elif diameter_mm <= 254.0:
            return float(1.51 * diameter_mm**(-0.157))
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
        return float((stress_amplitude_MPa / a) ** (1.0 / (-b)))


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
    def euler_critical_stress_MPa(
        youngs_modulus_GPa: float,
        slenderness_ratio: float,
    ) -> float:
        """Euler critical stress: sigma_cr = pi^2 * E / (KL/r)^2 [MPa].

        E in GPa -> convert to MPa.
        """
        E_MPa = youngs_modulus_GPa * 1000.0
        if slenderness_ratio <= 0:
            return float("inf")
        return math.pi**2 * E_MPa / slenderness_ratio**2

    @staticmethod
    def johnson_critical_stress_MPa(
        yield_strength_MPa: float,
        youngs_modulus_GPa: float,
        slenderness_ratio: float,
    ) -> float:
        """Johnson parabolic critical stress (Shigley Eq.4-43).

        sigma_cr = S_y - (S_y^2 / (4 * pi^2 * E)) * (KL/r)^2

        Valid for slenderness ratios below the transition point.
        """
        E_MPa = youngs_modulus_GPa * 1000.0
        return yield_strength_MPa - (
            yield_strength_MPa**2
            / (4.0 * math.pi**2 * E_MPa)
        ) * slenderness_ratio**2

    @staticmethod
    def transition_slenderness_ratio(
        yield_strength_MPa: float,
        youngs_modulus_GPa: float,
    ) -> float:
        """Euler-Johnson transition slenderness (Shigley Eq.4-44).

        (KL/r)_tr = sqrt(2 * pi^2 * E / S_y)
        """
        E_MPa = youngs_modulus_GPa * 1000.0
        if yield_strength_MPa <= 0:
            return float("inf")
        return math.sqrt(2.0 * math.pi**2 * E_MPa / yield_strength_MPa)

    @staticmethod
    def critical_buckling_stress_MPa(
        yield_strength_MPa: float,
        youngs_modulus_GPa: float,
        slenderness_ratio: float,
    ) -> float:
        """Unified buckling stress: auto-selects Euler or Johnson.

        Uses Johnson for slenderness < transition, Euler for >= transition.
        """
        transition = BucklingAnalysis.transition_slenderness_ratio(
            yield_strength_MPa, youngs_modulus_GPa,
        )
        if slenderness_ratio < transition:
            return BucklingAnalysis.johnson_critical_stress_MPa(
                yield_strength_MPa, youngs_modulus_GPa, slenderness_ratio,
            )
        return BucklingAnalysis.euler_critical_stress_MPa(
            youngs_modulus_GPa, slenderness_ratio,
        )

    @staticmethod
    def buckling_safety_factor(
        critical_load_N: float,
        applied_load_N: float,
    ) -> float:
        """Buckling safety factor = P_cr / P_applied."""
        if applied_load_N <= 0:
            return float("inf")
        return critical_load_N / applied_load_N


# ---------------------------------------------------------------------------
# Stress Concentration Factors (Peterson/Shigley)
# ---------------------------------------------------------------------------

class StressConcentration:
    """Stress concentration factor K_t for geometric discontinuities."""

    @staticmethod
    def stepped_shaft(D_mm: float, d_mm: float, r_mm: float) -> float:
        """K_t for a stepped shaft in bending (Peterson/Shigley Fig. A-15-8).

        Uses the Neuber/Peterson curve-fit for round bar with shoulder fillet:
        K_t = A * (r/d)^b

        where A, b depend on D/d. This formulation is more robust than the
        polynomial form for the range of interest.

        Valid for 1.01 <= D/d <= 6.0, 0.005 <= r/d <= 0.30.
        """
        if d_mm <= 0 or r_mm <= 0 or D_mm <= d_mm:
            return 1.0

        ratio_Dd = D_mm / d_mm
        ratio_rd = r_mm / d_mm

        # Clamp to valid ranges
        ratio_Dd = max(1.01, min(6.0, ratio_Dd))
        ratio_rd = max(0.005, min(0.30, ratio_rd))

        # Peterson power-law fit: K_t = A * (r/d)^b
        # where A and b depend on D/d (interpolated from tabulated data).
        # Reference: Peterson's Stress Concentration Factors, 3rd ed., Chart 3.4
        if ratio_Dd <= 1.1:
            A_coeff, b_exp = 0.94, -0.30
        elif ratio_Dd <= 1.5:
            A_coeff, b_exp = 0.98, -0.29
        elif ratio_Dd <= 2.0:
            A_coeff, b_exp = 1.01, -0.28
        elif ratio_Dd <= 3.0:
            A_coeff, b_exp = 1.03, -0.27
        else:
            A_coeff, b_exp = 1.04, -0.26

        K_t = float(A_coeff * ratio_rd ** b_exp)
        return max(1.0, K_t)

    @staticmethod
    def keyway_bending() -> float:
        """K_t for keyway in bending (Shigley Table 7-1): K_t = 2.14."""
        return 2.14

    @staticmethod
    def keyway_torsion() -> float:
        """K_t for keyway in torsion (Shigley Table 7-1): K_t = 3.0."""
        return 3.0


# ---------------------------------------------------------------------------
# Dynamic Load Factor (AGMA, Shigley Ch.14)
# ---------------------------------------------------------------------------

class DynamicLoadFactor:
    """AGMA dynamic factor K_v for gear tooth bending stress."""

    @staticmethod
    def agma_Kv(pitch_line_velocity_ft_min: float, quality_number: int = 6) -> float:
        """AGMA dynamic factor K_v (Shigley Eq.14-27).

        K_v = ((A + sqrt(V)) / A)^B

        where A = 50 + 56*(1 - B), B = 0.25*(12 - Q_v)^(2/3).
        V in ft/min.

        At 30 RPM, 50mm pitch dia: V ~ 15.5 ft/min, K_v < 1.1.
        """
        Q_v = max(3, min(12, quality_number))
        B = 0.25 * (12 - Q_v) ** (2.0 / 3.0)
        A = 50.0 + 56.0 * (1.0 - B)

        V = max(0.0, pitch_line_velocity_ft_min)
        if V == 0.0:
            return 1.0

        K_v = float(((A + math.sqrt(V)) / A) ** B)
        return max(1.0, K_v)

    @staticmethod
    def pitch_velocity_ft_min(pitch_diameter_mm: float, rpm: float) -> float:
        """Convert metric pitch parameters to ft/min.

        V = pi * d * n / 12  (d in feet, n in rpm)
        """
        d_ft = pitch_diameter_mm / 304.8
        return math.pi * d_ft * rpm

    @staticmethod
    def agma_bending_stress_MPa(
        lewis_stress_MPa: float,
        Kv: float,
    ) -> float:
        """AGMA bending stress: sigma_AGMA = sigma_Lewis * K_v."""
        return lewis_stress_MPa * Kv


# ---------------------------------------------------------------------------
# Shaft Critical Speed (Rayleigh-Ritz)
# ---------------------------------------------------------------------------

class ShaftCriticalSpeed:
    """First critical speed of a rotating shaft (Shigley Ch.7)."""

    @staticmethod
    def first_critical_speed_rad_s(
        youngs_modulus_GPa: float,
        diameter_mm: float,
        length_mm: float,
        density_kg_m3: float,
    ) -> float:
        """Rayleigh-Ritz first critical speed (Shigley Eq.7-22).

        omega_n = (pi^2 / L^2) * sqrt(E*I / (rho*A))

        For a simply supported uniform shaft between two bearings.
        """
        d_m = diameter_mm / 1000.0
        L_m = length_mm / 1000.0
        E_Pa = youngs_modulus_GPa * 1e9
        I = math.pi * d_m**4 / 64.0
        A = math.pi * d_m**2 / 4.0

        if L_m <= 0 or A <= 0 or density_kg_m3 <= 0:
            return float("inf")

        return (math.pi**2 / L_m**2) * math.sqrt(E_Pa * I / (density_kg_m3 * A))

    @staticmethod
    def critical_speed_rpm(omega_n_rad_s: float) -> float:
        """Convert critical speed from rad/s to RPM."""
        return omega_n_rad_s * 60.0 / (2.0 * math.pi)

    @staticmethod
    def critical_speed_margin(critical_rpm: float, operating_rpm: float) -> float:
        """Safety margin: ratio of critical speed to operating speed.

        Should be > 3 for safe operation.
        """
        if operating_rpm <= 0:
            return float("inf")
        return critical_rpm / operating_rpm


# ---------------------------------------------------------------------------
# Notch Sensitivity (Neuber, Shigley Ch.6)
# ---------------------------------------------------------------------------

class NotchSensitivity:
    """Neuber notch sensitivity for fatigue concentration factors."""

    @staticmethod
    def neuber_constant_mm(ultimate_strength_MPa: float) -> float:
        """Neuber material constant 'a' in mm (Shigley Eq.6-35a).

        Empirical fit for steel: a = 0.246 - 3.08e-3*Su + 1.51e-5*Su^2 - 2.67e-8*Su^3
        Su in MPa, result in mm. Valid for 345 < Su < 1725 MPa.
        For cast iron/bronze, use larger a (less notch-sensitive).
        """
        Su = max(345.0, min(1725.0, ultimate_strength_MPa))
        a = (
            0.246
            - 3.08e-3 * Su
            + 1.51e-5 * Su**2
            - 2.67e-8 * Su**3
        )
        return max(0.001, a)

    @staticmethod
    def notch_sensitivity_q(
        notch_radius_mm: float,
        neuber_a_mm: float,
    ) -> float:
        """Neuber notch sensitivity q (Shigley Eq.6-33).

        q = 1 / (1 + a/r)

        where a is Neuber material constant and r is notch radius.
        q in [0, 1]: 0 = not sensitive, 1 = fully sensitive.
        """
        if notch_radius_mm <= 0:
            return 0.0
        return 1.0 / (1.0 + neuber_a_mm / notch_radius_mm)

    @staticmethod
    def fatigue_concentration_Kf(
        Kt: float,
        q: float,
    ) -> float:
        """Fatigue stress concentration factor (Shigley Eq.6-30).

        K_f = 1 + q * (K_t - 1)

        where q is notch sensitivity and K_t is theoretical SCF.
        """
        return 1.0 + q * (Kt - 1.0)

    @staticmethod
    def corrected_endurance_limit_MPa(
        Se_uncorrected_MPa: float,
        Kf: float,
    ) -> float:
        """Endurance limit corrected for fatigue concentration.

        Se_corrected = Se / K_f
        """
        if Kf <= 0:
            return 0.0
        return Se_uncorrected_MPa / Kf
