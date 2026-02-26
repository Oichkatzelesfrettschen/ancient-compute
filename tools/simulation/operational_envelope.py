#!/usr/bin/env python3
"""Operational Envelope -- sweep RPM and record failure modes.

Runs the SimulationEngine at various RPM values (10-120) and records:
  - Maximum temperature at steady state or failure
  - Time to first failure
  - Limiting component (temperature, clearance, seizure, backlash)
  - Maximum bearing wear at failure or end of run

Output: CSV to stdout suitable for gnuplot or spreadsheet analysis.

Usage:
    python3 tools/simulation/operational_envelope.py [--max-hours 100]
"""

from __future__ import annotations

import argparse
import csv
import sys

from backend.src.emulator.simulation.state import SimulationConfig
from backend.src.emulator.simulation.engine import SimulationEngine


def sweep_rpm(
    rpm_values: list[float],
    max_hours: float = 100.0,
) -> list[dict]:
    """Run simulation at each RPM and collect results."""
    results = []
    for rpm in rpm_values:
        config = SimulationConfig(rpm=rpm)
        engine = SimulationEngine(config)
        result = engine.predict_maintenance(max_hours=max_hours)

        max_wear = 0.0
        if result.final_state.bearing_wear_volumes_mm3:
            max_wear = max(result.final_state.bearing_wear_volumes_mm3)

        results.append({
            "rpm": rpm,
            "max_temp_C": result.final_state.temperature_C,
            "failure_time_s": result.failure_time_s,
            "limiting_component": result.limiting_component or "none",
            "max_wear_mm3": max_wear,
            "shaft_deflection_mm": result.final_state.shaft_deflection_mm,
            "gear_backlash_mm": result.final_state.gear_backlash_mm,
            "lubrication_regime": result.final_state.lubrication_regime,
            "energy_consumed_J": result.final_state.energy_consumed_J,
            "steps": result.steps,
        })
    return results


def main() -> None:
    parser = argparse.ArgumentParser(description="Operational Envelope sweep")
    parser.add_argument(
        "--max-hours", type=float, default=100.0,
        help="Maximum simulation hours per RPM point (default: 100)",
    )
    parser.add_argument(
        "--rpm-start", type=float, default=10.0,
        help="Starting RPM (default: 10)",
    )
    parser.add_argument(
        "--rpm-end", type=float, default=120.0,
        help="Ending RPM (default: 120)",
    )
    parser.add_argument(
        "--rpm-step", type=float, default=10.0,
        help="RPM increment (default: 10)",
    )
    args = parser.parse_args()

    rpm_values = []
    r = args.rpm_start
    while r <= args.rpm_end:
        rpm_values.append(r)
        r += args.rpm_step

    print(f"# Operational Envelope Sweep: {len(rpm_values)} RPM points, "
          f"max {args.max_hours}h each", file=sys.stderr)

    results = sweep_rpm(rpm_values, max_hours=args.max_hours)

    writer = csv.DictWriter(
        sys.stdout,
        fieldnames=[
            "rpm", "max_temp_C", "failure_time_s", "limiting_component",
            "max_wear_mm3", "shaft_deflection_mm", "gear_backlash_mm",
            "lubrication_regime", "energy_consumed_J", "steps",
        ],
    )
    writer.writeheader()
    for row in results:
        writer.writerow(row)


if __name__ == "__main__":
    main()
