#!/usr/bin/env python3
"""Monte Carlo tolerance stack-up analysis for Babbage Engine.

Runs 10,000 iterations with normally distributed parameter variations
to assess:
1. Gear backlash interference probability (gears binding)
2. Bearing clearance interference probability (shaft seizing)
3. Thermal expansion interference at max temperature
4. Structural safety factor distribution

Each parameter is sampled from N(nominal, sigma) where sigma = tolerance/3
(99.7% of parts within tolerance band).

Exit code 0: all interference probabilities < 0.1%.
Exit code 1: one or more probabilities exceed threshold.
"""

from __future__ import annotations

import math
import random
import sys
from dataclasses import dataclass
from typing import List, Tuple


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

N_ITERATIONS = 10_000
RANDOM_SEED = 42
INTERFERENCE_THRESHOLD = 0.001  # 0.1%


# ---------------------------------------------------------------------------
# Parameter distributions (nominal, tolerance_half_range)
# Tolerance = manufacturing capability for 19th century precision machining.
# Sigma = tolerance / 3 for 3-sigma process capability.
# ---------------------------------------------------------------------------

@dataclass
class ParamDist:
    name: str
    nominal: float
    tolerance: float  # +/- this amount

    @property
    def sigma(self) -> float:
        return self.tolerance / 3.0

    def sample(self, rng: random.Random) -> float:
        return rng.gauss(self.nominal, self.sigma)


# Gear parameters
GEAR_MODULE = ParamDist("gear_module_mm", 2.5, 0.05)  # +/- 50 um
GEAR_BACKLASH = ParamDist("backlash_mm", 0.15, 0.03)  # +/- 30 um
PRESSURE_ANGLE = ParamDist("pressure_angle_deg", 20.0, 0.5)  # +/- 0.5 deg

# Bearing parameters
BEARING_BORE = ParamDist("bearing_bore_mm", 50.0, 0.025)  # +/- 25 um
SHAFT_DIAMETER = ParamDist("shaft_diameter_mm", 49.9, 0.025)  # nominal clearance 0.1mm
BEARING_CLEARANCE_NOMINAL = 0.10  # mm (bore - shaft)

# Thermal parameters
BRASS_ALPHA = ParamDist("brass_alpha_per_K", 20.5e-6, 1.0e-6)
STEEL_ALPHA = ParamDist("steel_alpha_per_K", 11.7e-6, 0.5e-6)
AMBIENT_TEMP = ParamDist("ambient_temp_C", 20.0, 5.0)  # 15-25 C typical room
MAX_OPERATING_TEMP_C = 40.0

# Structural parameters
STEEL_YIELD = ParamDist("steel_yield_MPa", 275.0, 20.0)
SHAFT_LOAD = ParamDist("shaft_load_N", 500.0, 50.0)
GEAR_TANGENTIAL_LOAD = ParamDist("gear_load_N", 318.0, 30.0)  # from torque calc


# ---------------------------------------------------------------------------
# Monte Carlo checks
# ---------------------------------------------------------------------------

@dataclass
class MCResult:
    name: str
    n_iterations: int
    n_failures: int
    failure_probability: float
    mean_margin: float
    min_margin: float
    p99_margin: float


def check_gear_backlash(rng: random.Random, n: int) -> MCResult:
    """Check probability of gear teeth interfering (backlash < 0).

    Interference occurs when thermal expansion reduces backlash to zero.
    """
    failures = 0
    margins: List[float] = []

    for _ in range(n):
        backlash = GEAR_BACKLASH.sample(rng)
        module = GEAR_MODULE.sample(rng)
        brass_alpha = BRASS_ALPHA.sample(rng)
        steel_alpha = STEEL_ALPHA.sample(rng)
        temp = rng.gauss(MAX_OPERATING_TEMP_C, 2.0)  # near max operating temp

        # Pitch diameter ~ module * 20 teeth = 50 mm nominal
        pitch_d_mm = module * 20.0
        delta_T = temp - 20.0  # from assembly temp

        # Brass gear expands more than steel frame
        # alpha [1/K] * L [mm] * dT [K] = expansion [mm]
        # (alpha is dimensionless per K, so mm * alpha * K = mm)
        expansion_mm = brass_alpha * pitch_d_mm * delta_T
        # Backlash reduced by differential expansion
        effective_backlash = backlash - abs(expansion_mm)

        margins.append(effective_backlash)
        if effective_backlash <= 0:
            failures += 1

    margins.sort()
    return MCResult(
        name="Gear backlash interference",
        n_iterations=n,
        n_failures=failures,
        failure_probability=failures / n,
        mean_margin=sum(margins) / n,
        min_margin=margins[0],
        p99_margin=margins[int(0.01 * n)],
    )


def check_bearing_clearance(rng: random.Random, n: int) -> MCResult:
    """Check probability of shaft seizing in bearing (clearance < 0).

    Interference occurs when thermal expansion closes the clearance gap.
    """
    failures = 0
    margins: List[float] = []

    for _ in range(n):
        bore = BEARING_BORE.sample(rng)
        shaft = SHAFT_DIAMETER.sample(rng)
        steel_alpha = STEEL_ALPHA.sample(rng)
        brass_alpha = BRASS_ALPHA.sample(rng)
        temp = rng.gauss(MAX_OPERATING_TEMP_C, 2.0)

        # Cold clearance
        cold_clearance = bore - shaft
        delta_T = temp - 20.0

        # Steel shaft expands; bronze bearing expands more
        # Net clearance change = bearing_expansion - shaft_expansion
        # Bronze bearing (phosphor bronze alpha ~ brass)
        # alpha [1/K] * L [mm] * dT [K] = expansion [mm]
        shaft_growth = steel_alpha * shaft * delta_T
        bearing_growth = brass_alpha * bore * delta_T
        hot_clearance = cold_clearance + (bearing_growth - shaft_growth)

        margins.append(hot_clearance)
        if hot_clearance <= 0:
            failures += 1

    margins.sort()
    return MCResult(
        name="Bearing clearance interference",
        n_iterations=n,
        n_failures=failures,
        failure_probability=failures / n,
        mean_margin=sum(margins) / n,
        min_margin=margins[0],
        p99_margin=margins[int(0.01 * n)],
    )


def check_structural_safety(rng: random.Random, n: int) -> MCResult:
    """Check probability of structural safety factor < 2.0.

    Uses simplified gear tooth bending stress vs yield strength.
    """
    failures = 0
    margins: List[float] = []

    for _ in range(n):
        yield_mpa = STEEL_YIELD.sample(rng)
        load_n = GEAR_TANGENTIAL_LOAD.sample(rng)
        module = GEAR_MODULE.sample(rng)
        face_width = rng.gauss(15.0, 0.5)  # +/- 0.5 mm

        Y = 0.32  # Lewis form factor
        stress_mpa = load_n / (face_width * module * Y)
        sf = yield_mpa / stress_mpa

        margins.append(sf - 2.0)  # margin above SF=2.0
        if sf < 2.0:
            failures += 1

    margins.sort()
    return MCResult(
        name="Structural safety factor < 2.0",
        n_iterations=n,
        n_failures=failures,
        failure_probability=failures / n,
        mean_margin=sum(margins) / n,
        min_margin=margins[0],
        p99_margin=margins[int(0.01 * n)],
    )


def check_shaft_deflection(rng: random.Random, n: int) -> MCResult:
    """Check probability of shaft deflection exceeding L/10000."""
    failures = 0
    margins: List[float] = []

    for _ in range(n):
        load = SHAFT_LOAD.sample(rng)
        diameter = rng.gauss(50.0, 0.025 / 3.0)
        span = rng.gauss(375.0, 1.0)
        E_GPa = rng.gauss(205.0, 5.0)

        L_m = span / 1000.0
        E_Pa = E_GPa * 1e9
        d_m = diameter / 1000.0
        I = math.pi * d_m**4 / 64.0
        delta_m = load * L_m**3 / (48.0 * E_Pa * I)
        delta_mm = delta_m * 1000.0

        limit_mm = span / 10000.0  # L/10000
        margin = limit_mm - delta_mm

        margins.append(margin)
        if delta_mm > limit_mm:
            failures += 1

    margins.sort()
    return MCResult(
        name="Shaft deflection > L/10000",
        n_iterations=n,
        n_failures=failures,
        failure_probability=failures / n,
        mean_margin=sum(margins) / n,
        min_margin=margins[0],
        p99_margin=margins[int(0.01 * n)],
    )


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    rng = random.Random(RANDOM_SEED)
    n = N_ITERATIONS

    print(f"Monte Carlo Tolerance Stack-up Analysis")
    print(f"Iterations: {n}")
    print(f"Interference threshold: {INTERFERENCE_THRESHOLD * 100:.1f}%")
    print(f"Random seed: {RANDOM_SEED}")
    print()

    checks = [
        check_gear_backlash(rng, n),
        check_bearing_clearance(rng, n),
        check_structural_safety(rng, n),
        check_shaft_deflection(rng, n),
    ]

    any_failed = False
    for result in checks:
        status = "PASS" if result.failure_probability < INTERFERENCE_THRESHOLD else "FAIL"
        if status == "FAIL":
            any_failed = True
        print(f"[{status}] {result.name}")
        print(f"  Failures:         {result.n_failures}/{result.n_iterations} ({result.failure_probability * 100:.3f}%)")
        print(f"  Mean margin:      {result.mean_margin:.6f}")
        print(f"  Min margin:       {result.min_margin:.6f}")
        print(f"  P1 margin:        {result.p99_margin:.6f}")
        print()

    if any_failed:
        print("RESULT: One or more checks exceed interference threshold.")
        return 1
    else:
        print("RESULT: All interference probabilities below threshold. Mechanical feasibility confirmed.")
        return 0


if __name__ == "__main__":
    sys.exit(main())
