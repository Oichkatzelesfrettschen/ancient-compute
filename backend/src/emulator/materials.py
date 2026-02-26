"""
Material Property Library for Babbage Engine Physics Simulation

Loads material properties from sim_schema.yaml and provides typed access
to mechanical, thermal, and electromagnetic properties for all 5 construction
materials. Property values sourced from engineering handbooks (Shigley's,
ASM, Eurocode, copper.org). Valid for 10-40 C ambient.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Union

import yaml

SCHEMA_PATH = Path(__file__).resolve().parents[3] / "docs" / "simulation" / "sim_schema.yaml"


@dataclass(frozen=True)
class MaterialProperties:
    """Immutable container for a single material's engineering properties."""

    name: str
    designation: str
    assignment: str
    density_kg_m3: float
    friction_coeff: float
    youngs_modulus_GPa: tuple[float, float]
    poissons_ratio: float
    yield_strength_MPa: tuple[float, float]
    ultimate_tensile_strength_MPa: tuple[float, float]
    endurance_limit_MPa: tuple[float, float]
    thermal_expansion_coeff_per_K: float
    specific_heat_J_kgK: float
    thermal_conductivity_W_mK: float
    hardness_HB: tuple[float, float]
    electrical_resistivity_ohm_m: float
    magnetic_permeability_relative: float
    creep_threshold_C: float
    temperature_range_C: tuple[float, float]
    source: str
    # Optional fields present on some materials
    compressive_strength_MPa: float | None = None
    notes: str | None = None

    # -- Derived convenience accessors --

    @property
    def youngs_modulus_Pa(self) -> tuple[float, float]:
        """Young's modulus in Pascals (SI)."""
        return (self.youngs_modulus_GPa[0] * 1e9, self.youngs_modulus_GPa[1] * 1e9)

    @property
    def yield_strength_Pa(self) -> tuple[float, float]:
        """Yield strength in Pascals (SI)."""
        return (self.yield_strength_MPa[0] * 1e6, self.yield_strength_MPa[1] * 1e6)

    @property
    def ultimate_tensile_strength_Pa(self) -> tuple[float, float]:
        """UTS in Pascals (SI)."""
        return (
            self.ultimate_tensile_strength_MPa[0] * 1e6,
            self.ultimate_tensile_strength_MPa[1] * 1e6,
        )

    @property
    def endurance_limit_Pa(self) -> tuple[float, float]:
        """Endurance limit in Pascals (SI)."""
        return (self.endurance_limit_MPa[0] * 1e6, self.endurance_limit_MPa[1] * 1e6)

    @property
    def shear_modulus_GPa(self) -> tuple[float, float]:
        """Shear modulus G = E / (2*(1+nu)), in GPa."""
        denom = 2.0 * (1.0 + self.poissons_ratio)
        return (self.youngs_modulus_GPa[0] / denom, self.youngs_modulus_GPa[1] / denom)

    def safety_factor_yield(self, applied_stress_MPa: float) -> float:
        """Safety factor against yield using minimum yield strength."""
        if applied_stress_MPa <= 0:
            return float("inf")
        return self.yield_strength_MPa[0] / applied_stress_MPa

    def safety_factor_fatigue(self, stress_amplitude_MPa: float) -> float:
        """Safety factor against fatigue using minimum endurance limit."""
        if stress_amplitude_MPa <= 0:
            return float("inf")
        return self.endurance_limit_MPa[0] / stress_amplitude_MPa


def _to_range(value: Any) -> tuple[float, float]:
    """Convert a scalar or [min, max] list to a (min, max) tuple."""
    if isinstance(value, list):
        if len(value) != 2:
            raise ValueError(f"Expected [min, max], got list of length {len(value)}")
        return (float(value[0]), float(value[1]))
    return (float(value), float(value))


def _parse_material(entry: dict[str, Any]) -> MaterialProperties:
    """Parse a single material entry from the YAML schema."""
    return MaterialProperties(
        name=entry["name"],
        designation=entry.get("designation", ""),
        assignment=entry.get("assignment", ""),
        density_kg_m3=float(entry["density_kg_m3"]),
        friction_coeff=float(entry["friction_coeff"]),
        youngs_modulus_GPa=_to_range(entry["youngs_modulus_GPa"]),
        poissons_ratio=float(entry["poissons_ratio"]),
        yield_strength_MPa=_to_range(entry["yield_strength_MPa"]),
        ultimate_tensile_strength_MPa=_to_range(entry["ultimate_tensile_strength_MPa"]),
        endurance_limit_MPa=_to_range(entry["endurance_limit_MPa"]),
        thermal_expansion_coeff_per_K=float(entry["thermal_expansion_coeff_per_K"]),
        specific_heat_J_kgK=float(entry["specific_heat_J_kgK"]),
        thermal_conductivity_W_mK=float(entry["thermal_conductivity_W_mK"]),
        hardness_HB=_to_range(entry["hardness_HB"]),
        electrical_resistivity_ohm_m=float(entry["electrical_resistivity_ohm_m"]),
        magnetic_permeability_relative=float(entry["magnetic_permeability_relative"]),
        creep_threshold_C=float(entry["creep_threshold_C"]),
        temperature_range_C=_to_range(entry["temperature_range_C"]),
        source=entry.get("source", ""),
        compressive_strength_MPa=(
            float(entry["compressive_strength_MPa"])
            if "compressive_strength_MPa" in entry
            else None
        ),
        notes=entry.get("notes"),
    )


class MaterialLibrary:
    """Load and query material properties from the simulation schema.

    Usage:
        lib = MaterialLibrary()          # loads from default schema path
        brass = lib.get("brass")         # -> MaterialProperties
        all_names = lib.names()          # -> ["brass", "steel", ...]
    """

    def __init__(self, schema_path: Union[str, Path] | None = None) -> None:
        path = Path(schema_path) if schema_path else SCHEMA_PATH
        if not path.exists():
            raise FileNotFoundError(f"Schema file not found: {path}")

        with path.open("r", encoding="utf-8") as fh:
            data = yaml.safe_load(fh)

        if not isinstance(data, dict) or "materials" not in data:
            raise ValueError(f"Schema at {path} missing 'materials' key")

        self._materials: dict[str, MaterialProperties] = {}
        for entry in data["materials"]:
            mat = _parse_material(entry)
            self._materials[mat.name] = mat

    def get(self, name: str) -> MaterialProperties:
        """Retrieve properties for a named material. Raises KeyError if unknown."""
        if name not in self._materials:
            available = ", ".join(sorted(self._materials.keys()))
            raise KeyError(f"Unknown material '{name}'. Available: {available}")
        return self._materials[name]

    def names(self) -> list[str]:
        """Return sorted list of available material names."""
        return sorted(self._materials.keys())

    def all_materials(self) -> list[MaterialProperties]:
        """Return all MaterialProperties objects, sorted by name."""
        return [self._materials[n] for n in self.names()]

    def __len__(self) -> int:
        return len(self._materials)

    def __contains__(self, name: str) -> bool:
        return name in self._materials

    def __repr__(self) -> str:
        return f"MaterialLibrary({len(self)} materials: {', '.join(self.names())})"
