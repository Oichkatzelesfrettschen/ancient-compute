#!/usr/bin/env python3
"""Bill of Materials cost/mass model for the Analytical Engine.

Estimates mass and material cost for major subsystems based on the
SimulationConfig geometry and MaterialLibrary density values.

Usage:
    PYTHONPATH=. python3 tools/simulation/bom_cost_model.py
"""

from __future__ import annotations

import math

from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.simulation.state import SimulationConfig


def estimate_bom(config: SimulationConfig) -> list[dict]:
    """Estimate mass and cost for major subsystems."""
    lib = MaterialLibrary()
    items = []

    # Helper: cylinder volume in mm3 -> kg
    def cylinder_mass_kg(diameter_mm: float, length_mm: float, material: str) -> float:
        r = diameter_mm / 2.0
        vol_mm3 = math.pi * r**2 * length_mm
        rho = lib.get(material).density_kg_m3  # kg/m3
        return vol_mm3 * 1e-9 * rho  # mm3 -> m3

    # 1. Main shaft
    shaft_mass = cylinder_mass_kg(
        config.shaft_diameter_mm, config.shaft_length_mm, config.shaft_material,
    )
    items.append({
        "subsystem": "Main shaft",
        "material": config.shaft_material,
        "mass_kg": shaft_mass,
        "count": 1,
    })

    # 2. Bearings (journal bearings)
    bearing_od = config.shaft_diameter_mm + 2 * 5.0  # 5mm wall thickness
    bearing_mass = (
        cylinder_mass_kg(bearing_od, config.bearing_length_mm, config.bearing_material)
        - cylinder_mass_kg(config.shaft_diameter_mm, config.bearing_length_mm, config.bearing_material)
    )
    items.append({
        "subsystem": "Journal bearings",
        "material": config.bearing_material,
        "mass_kg": bearing_mass,
        "count": config.bearing_count,
    })

    # 3. Spur gears (approximate as solid disc)
    gear_od = config.gear_tooth_count * config.gear_module_mm
    gear_mass = cylinder_mass_kg(
        gear_od, config.gear_face_width_mm, config.gear_material,
    )
    items.append({
        "subsystem": "Spur gears",
        "material": config.gear_material,
        "mass_kg": gear_mass,
        "count": 8,  # Approximate gear count
    })

    # 4. Frame (cast iron, approximate as fraction of total machine mass)
    frame_mass = config.machine_mass_kg * 0.6  # 60% of machine is frame
    items.append({
        "subsystem": "Frame (cast iron)",
        "material": "cast_iron",
        "mass_kg": frame_mass,
        "count": 1,
    })

    # 5. Digit columns (brass wheels)
    # Babbage planned 1000 columns of 50 digits each
    wheel_mass = cylinder_mass_kg(30.0, 5.0, "brass")  # 30mm dia, 5mm thick
    items.append({
        "subsystem": "Digit wheels",
        "material": "brass",
        "mass_kg": wheel_mass,
        "count": 50000,  # 1000 columns x 50 digits
    })

    return items


def print_bom(items: list[dict]) -> None:
    """Print BOM table to stdout."""
    print(f"{'Subsystem':<25s} {'Material':<20s} {'Count':>6s} {'Unit kg':>10s} {'Total kg':>10s}")
    print("-" * 75)
    total_mass = 0.0
    for item in items:
        total = item["mass_kg"] * item["count"]
        total_mass += total
        print(
            f"{item['subsystem']:<25s} "
            f"{item['material']:<20s} "
            f"{item['count']:>6d} "
            f"{item['mass_kg']:>10.3f} "
            f"{total:>10.3f}"
        )
    print("-" * 75)
    print(f"{'TOTAL':<25s} {'':20s} {'':>6s} {'':>10s} {total_mass:>10.3f}")


def main() -> None:
    config = SimulationConfig()
    items = estimate_bom(config)
    print_bom(items)


if __name__ == "__main__":
    main()
