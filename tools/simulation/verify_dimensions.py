#!/usr/bin/env python3
"""Dimensional analysis verification for all physics model equations.

Verifies that every equation in the physics model (Phases A-F) produces
correct SI units. Each check encodes the left-hand-side unit and derives
it from the right-hand-side operand units.

Exit code 0: all checks pass.
Exit code 1: one or more dimensional inconsistencies found.
"""

from __future__ import annotations

import sys


# ---------------------------------------------------------------------------
# Lightweight unit representation
# ---------------------------------------------------------------------------
# We encode SI base dimensions as a tuple:
#   (kg, m, s, K, A)
# For example: Force [N] = kg*m/s^2 => (1, 1, -2, 0, 0)

DIMENSIONLESS = (0, 0, 0, 0, 0)
METER    = (0, 1, 0, 0, 0)
KILOGRAM = (1, 0, 0, 0, 0)
SECOND   = (0, 0, 1, 0, 0)
KELVIN   = (0, 0, 0, 1, 0)
AMPERE   = (0, 0, 0, 0, 1)

# Derived units
NEWTON   = (1, 1, -2, 0, 0)     # kg*m/s^2
PASCAL   = (1, -1, -2, 0, 0)    # N/m^2 = kg/(m*s^2)
JOULE    = (1, 2, -2, 0, 0)     # N*m = kg*m^2/s^2
WATT     = (1, 2, -3, 0, 0)     # J/s = kg*m^2/s^3
RAD_PER_S = (0, 0, -1, 0, 0)    # rad/s (radians are dimensionless)
M_PER_S  = (0, 1, -1, 0, 0)     # m/s
M2       = (0, 2, 0, 0, 0)      # m^2
M3       = (0, 3, 0, 0, 0)      # m^3
M4       = (0, 4, 0, 0, 0)      # m^4
PER_K    = (0, 0, 0, -1, 0)     # 1/K
J_PER_KG_K = (0, 2, -2, -1, 0)  # J/(kg*K) = m^2/(s^2*K)
W_PER_M_K = (1, 1, -3, -1, 0)   # W/(m*K) = kg*m/(s^3*K)
OHM_M    = (1, 3, -3, 0, -2)    # Ohm*m = kg*m^3/(s^3*A^2)
TESLA    = (1, 0, -2, 0, -1)    # T = kg/(s^2*A)


def mul(*units):
    """Multiply units (add exponent tuples)."""
    result = [0, 0, 0, 0, 0]
    for u in units:
        for i in range(5):
            result[i] += u[i]
    return tuple(result)


def div(num, den):
    """Divide units (subtract exponent tuples)."""
    return tuple(num[i] - den[i] for i in range(5))


def power(unit, exp):
    """Raise unit to a power."""
    return tuple(u * exp for u in unit)


def unit_name(u):
    """Human-readable unit string."""
    names = ["kg", "m", "s", "K", "A"]
    parts = []
    for i, n in enumerate(names):
        if u[i] != 0:
            if u[i] == 1:
                parts.append(n)
            else:
                parts.append(f"{n}^{u[i]}")
    return " * ".join(parts) if parts else "dimensionless"


# ---------------------------------------------------------------------------
# Equation checks
# ---------------------------------------------------------------------------

def check(equation_name: str, expected, computed) -> bool:
    """Check that computed unit matches expected unit."""
    if expected == computed:
        return True
    print(f"  FAIL: {equation_name}")
    print(f"    Expected: {unit_name(expected)}")
    print(f"    Computed: {unit_name(computed)}")
    return False


def verify_all() -> int:
    failures = 0
    total = 0

    # === Phase B: Kinematics ===

    # Gear velocity ratio: omega_2/omega_1 = N_1/N_2 [dimensionless]
    total += 1
    ratio_unit = div(RAD_PER_S, RAD_PER_S)
    if not check("Gear velocity ratio (omega_2/omega_1)", DIMENSIONLESS, ratio_unit):
        failures += 1

    # Contact ratio: CR = [lengths] / [length] => dimensionless
    # CR = (sqrt(r_a1^2 - r_b1^2) + sqrt(r_a2^2 - r_b2^2) - c*sin(phi)) / (pi*m*cos(phi))
    # Numerator: [m], Denominator: [m] => dimensionless
    total += 1
    cr_num = METER  # meters
    cr_den = METER  # module [m]
    cr_unit = div(cr_num, cr_den)
    if not check("Contact ratio (CR)", DIMENSIONLESS, cr_unit):
        failures += 1

    # Lewis bending stress: sigma_b = W_t / (b * m * Y) [Pa]
    # W_t [N], b [m], m [m], Y [dimensionless]
    total += 1
    lewis_num = NEWTON
    lewis_den = mul(METER, METER, DIMENSIONLESS)
    lewis_unit = div(lewis_num, lewis_den)
    if not check("Lewis bending stress (sigma_b = W_t/(b*m*Y))", PASCAL, lewis_unit):
        failures += 1

    # Pitch line velocity: v = pi * d * n / 60 [m/s]
    # d [m], n [rev/s] (dimensionally 1/s since rev is dimensionless)
    total += 1
    plv_unit = mul(METER, RAD_PER_S)  # m * (1/s) = m/s
    if not check("Pitch line velocity (v = pi*d*n)", M_PER_S, plv_unit):
        failures += 1

    # Moment of inertia: I = pi*d^4/64 [m^4]
    total += 1
    i_unit = power(METER, 4)
    if not check("Moment of inertia (I = pi*d^4/64)", M4, i_unit):
        failures += 1

    # Grubler-Kutzbach: M = 3(n-1) - 2*j1 - j2 [dimensionless]
    total += 1
    gk_unit = DIMENSIONLESS  # integer counts
    if not check("Grubler-Kutzbach DOF (M)", DIMENSIONLESS, gk_unit):
        failures += 1

    # Cam displacement: s(theta) = h * [theta/beta - ...] [m]
    # h [m], theta/beta [dimensionless] => [m]
    total += 1
    cam_unit = mul(METER, DIMENSIONLESS)
    if not check("Cam displacement (s = h * f(theta))", METER, cam_unit):
        failures += 1

    # === Phase C: Thermodynamics ===

    # Bearing heat: Q = 0.5 * mu * W * d * omega [W]
    # mu [dimless], W [N], d [m], omega [rad/s]
    # N * m / s = kg*m^2/s^3 = W
    total += 1
    bearing_heat_unit = mul(DIMENSIONLESS, NEWTON, METER, RAD_PER_S)
    if not check("Bearing heat (Q = 0.5*mu*W*d*omega)", WATT, bearing_heat_unit):
        failures += 1

    # Gear mesh heat: Q = P * (1 - eta) [W]
    # P [W], eta [dimensionless]
    total += 1
    gear_heat_unit = mul(WATT, DIMENSIONLESS)
    if not check("Gear mesh heat (Q = P*(1-eta))", WATT, gear_heat_unit):
        failures += 1

    # Thermal expansion: delta_L = alpha * L * delta_T [m]
    # alpha [1/K], L [m], delta_T [K]
    total += 1
    expansion_unit = mul(PER_K, METER, KELVIN)
    if not check("Thermal expansion (delta_L = alpha*L*dT)", METER, expansion_unit):
        failures += 1

    # Thermal time constant: tau = m * c_p / (h * A) [s]
    # m [kg], c_p [J/(kg*K)], h [W/(m^2*K)], A [m^2]
    # Numerator: kg * J/(kg*K) = J/K = kg*m^2/(s^2*K)
    # Denominator: W/(m^2*K) * m^2 = W/K = kg*m^2/(s^3*K)
    # Result: [kg*m^2/(s^2*K)] / [kg*m^2/(s^3*K)] = s
    total += 1
    tau_num = mul(KILOGRAM, J_PER_KG_K)  # kg * m^2/(s^2*K) = m^2*kg/(s^2*K)
    W_PER_M2_K = (1, 0, -3, -1, 0)  # W/(m^2*K)
    tau_den = mul(W_PER_M2_K, M2)  # W/K = kg*m^2/(s^3*K)
    tau_unit = div(tau_num, tau_den)
    if not check("Thermal time constant (tau = m*cp/(h*A))", SECOND, tau_unit):
        failures += 1

    # === Phase D: Electromagnetic ===

    # Eddy current loss: P = pi^2 * B^2 * d^2 * f^2 * V_vol / (6 * rho_e) [W]
    # B [T=kg/(s^2*A)], d [m], f [1/s], V_vol [m^3], rho_e [Ohm*m=kg*m^3/(s^3*A^2)]
    # B^2 = kg^2/(s^4*A^2)
    # B^2 * d^2 * f^2 * V_vol = kg^2/(s^4*A^2) * m^2 * 1/s^2 * m^3
    #   = kg^2 * m^5 / (s^6 * A^2)
    # Divide by rho_e = kg*m^3/(s^3*A^2):
    #   = kg^2*m^5/(s^6*A^2) * s^3*A^2/(kg*m^3)
    #   = kg * m^2 / s^3 = W
    total += 1
    b2 = power(TESLA, 2)
    d2 = power(METER, 2)
    f2 = power(RAD_PER_S, 2)  # frequency has same dimension as 1/s
    eddy_num = mul(b2, d2, f2, M3)
    eddy_unit = div(eddy_num, OHM_M)
    if not check("Eddy current loss (P = pi^2*B^2*d^2*f^2*V/(6*rho))", WATT, eddy_unit):
        failures += 1

    # === Phase E: Tribology ===

    # Archard wear: V = K * F_N * s / H [m^3]
    # K [dimless], F_N [N], s [m], H [Pa=N/m^2]
    # N * m / (N/m^2) = m^3
    total += 1
    archard_num = mul(DIMENSIONLESS, NEWTON, METER)
    archard_unit = div(archard_num, PASCAL)
    if not check("Archard wear (V = K*F*s/H)", M3, archard_unit):
        failures += 1

    # PV product: P * V [Pa * m/s]
    # P [Pa], V [m/s]
    total += 1
    PV_UNIT = (1, 0, -3, 0, 0)  # Pa*m/s = kg/(m*s^2) * m/s = kg/s^3
    pv_computed = mul(PASCAL, M_PER_S)
    if not check("PV product (P*V)", PV_UNIT, pv_computed):
        failures += 1

    # Lambda ratio: lambda = h_min / sqrt(Ra1^2 + Ra2^2) [dimensionless]
    # h_min [m], Ra [m] => m/m = dimless
    total += 1
    lambda_unit = div(METER, METER)
    if not check("Lambda ratio (h/Ra)", DIMENSIONLESS, lambda_unit):
        failures += 1

    # === Phase F: Structural ===

    # Beam deflection: delta = F*L^3 / (48*E*I) [m]
    # F [N], L^3 [m^3], E [Pa=N/m^2], I [m^4]
    # N*m^3 / (N/m^2 * m^4) = N*m^3 / (N*m^2) = m
    total += 1
    defl_num = mul(NEWTON, power(METER, 3))
    defl_den = mul(PASCAL, M4)
    defl_unit = div(defl_num, defl_den)
    if not check("Beam deflection (delta = F*L^3/(48*E*I))", METER, defl_unit):
        failures += 1

    # Goodman fatigue: sigma_a/sigma_e + sigma_m/sigma_u = 1/SF [dimensionless]
    # All sigma terms [Pa], SF [dimless]
    total += 1
    goodman_unit = div(PASCAL, PASCAL)
    if not check("Goodman fatigue (sigma_a/sigma_e + sigma_m/sigma_u)", DIMENSIONLESS, goodman_unit):
        failures += 1

    # Euler buckling: P_cr = pi^2*E*I / (K*L)^2 [N]
    # E [Pa=N/m^2], I [m^4], (K*L)^2 [m^2]
    # (N/m^2 * m^4) / m^2 = N
    total += 1
    euler_num = mul(PASCAL, M4)
    euler_den = M2
    euler_unit = div(euler_num, euler_den)
    if not check("Euler buckling (P_cr = pi^2*E*I/(K*L)^2)", NEWTON, euler_unit):
        failures += 1

    # Summary
    print(f"\nDimensional analysis: {total - failures}/{total} checks passed.")
    return 1 if failures > 0 else 0


if __name__ == "__main__":
    sys.exit(verify_all())
