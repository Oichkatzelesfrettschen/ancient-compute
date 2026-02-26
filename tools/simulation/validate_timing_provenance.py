#!/usr/bin/env python3
"""Validate timing parameter provenance.

Reads TIMING_PROVISIONAL.yaml and reports the source status of every
timing parameter: provisional (needs primary-source measurement) or
confirmed (sourced from primary reference).

Exit code 0 if all parameters have non-empty sources; exit code 1 otherwise.
"""

import sys
from pathlib import Path

import yaml


TIMING_YAML = (
    Path(__file__).parent.parent.parent
    / "docs"
    / "simulation"
    / "TIMING_PROVISIONAL.yaml"
)


def load_timing_config():
    with open(TIMING_YAML) as f:
        return yaml.safe_load(f)


def walk_parameters(config, prefix=""):
    """Yield (dotted_path, param_dict) for every leaf with 'value'."""
    if isinstance(config, dict):
        if "value" in config and "provisional" in config:
            yield prefix, config
        elif "parameters" in config:
            for key, val in config["parameters"].items():
                yield from walk_parameters(val, f"{prefix}.{key}" if prefix else key)
        else:
            for key, val in config.items():
                yield from walk_parameters(val, f"{prefix}.{key}" if prefix else key)


def main():
    config = load_timing_config()

    provisional_count = 0
    confirmed_count = 0
    missing_source = 0

    print("Timing Parameter Provenance Report")
    print("=" * 70)
    print()

    for section_key in ["valve_gear", "mechanical_phases", "steam_drive", "barrel_timing"]:
        section = config.get(section_key)
        if section is None:
            continue

        print(f"[{section_key}]")
        for path, param in walk_parameters(section, section_key):
            prov = param.get("provisional", False)
            source = param.get("source", "")
            value = param.get("value")
            unit = param.get("unit", "")
            status = "PROVISIONAL" if prov else "CONFIRMED"

            if prov:
                provisional_count += 1
            else:
                confirmed_count += 1

            if not source:
                missing_source += 1
                status = "MISSING SOURCE"

            print(f"  {path:40s}  {str(value):>8s} {unit:>8s}  [{status}]")
            if source:
                print(f"    Source: {source}")
        print()

    print("=" * 70)
    print(f"  Confirmed:   {confirmed_count}")
    print(f"  Provisional: {provisional_count}")
    print(f"  Missing:     {missing_source}")
    print(f"  Total:       {confirmed_count + provisional_count}")
    print()

    if missing_source > 0:
        print("FAIL: Some parameters have no source attribution.")
        return 1
    else:
        print("OK: All parameters have source attribution.")
        return 0


if __name__ == "__main__":
    sys.exit(main())
