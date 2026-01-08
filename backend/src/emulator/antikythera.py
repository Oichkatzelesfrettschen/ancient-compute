"""Tier-2-ish gear-train propagation model for Antikythera-like mechanisms.

This is a generic gear train model (ratio propagation). Real Antikythera gear
ratios and dial mappings must be sourced and loaded separately.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import yaml


@dataclass(frozen=True)
class GearEdge:
    src: str
    dst: str
    ratio: float


class GearTrain:
    def __init__(self, edges: list[GearEdge]) -> None:
        self._edges = edges

    @staticmethod
    def mesh_ratio(teeth_src: int, teeth_dst: int) -> float:
        """Return angular ratio for a simple meshing gear pair.

        A meshing pair reverses direction (sign flip).
        """
        if teeth_src <= 0 or teeth_dst <= 0:
            raise ValueError("teeth counts must be positive")
        return -(teeth_src / teeth_dst)

    @classmethod
    def from_meshes(cls, meshes: Iterable[tuple[str, str, int, int]]) -> "GearTrain":
        edges = [GearEdge(src=a, dst=b, ratio=cls.mesh_ratio(ta, tb)) for (a, b, ta, tb) in meshes]
        return cls(edges)

    @classmethod
    def from_toothcount_yaml(cls, path: Path, *, meshes: list[tuple[str, str, str, str]]) -> "GearTrain":
        """Load tooth counts and build a train from gear-name meshes.

        meshes is a list of (src_gear, dst_gear, src_key, dst_key) where src_key/dst_key
        are keys in the YAML's 'gears' map (e.g. 'b1', 'a1').
        """
        with open(path, "r", encoding="utf-8") as handle:
            data = yaml.safe_load(handle)
        gears = data["gears"]
        resolved: list[tuple[str, str, int, int]] = []
        for src_gear, dst_gear, src_key, dst_key in meshes:
            resolved.append((
                src_gear,
                dst_gear,
                int(gears[src_key]["teeth"]),
                int(gears[dst_key]["teeth"]),
            ))
        return cls.from_meshes(resolved)

    def propagate(self, input_angle: float, input_gear: str) -> dict[str, float]:
        angles: dict[str, float] = {input_gear: input_angle}
        updated = True
        while updated:
            updated = False
            for edge in self._edges:
                if edge.src in angles and edge.dst not in angles:
                    angles[edge.dst] = angles[edge.src] * edge.ratio
                    updated = True
        return angles


def draconic_pointer_train_from_arxiv_2104_06181() -> GearTrain:
    """A minimal draconic-pointer train derived from arXiv:2104.06181.

    Models the (b1↔a1) mesh and the (r1↔s1) mesh, assuming r1 shares the a-shaft.
    """
    yaml_path = Path(__file__).resolve().parents[3] / "docs/sources/antikythera/draconic_gearing_arxiv_2104_06181.yaml"
    return GearTrain.from_toothcount_yaml(
        yaml_path,
        meshes=[
            ("b1", "a1", "b1", "a1"),
            ("r1", "s1", "r1", "s1"),
        ],
    )


class AntikytheraDraconicModel:
    """Minimal dial-level model for the Draconic pointer (Fragment D hypothesis)."""

    def __init__(self, train: GearTrain | None = None) -> None:
        self._train = train or draconic_pointer_train_from_arxiv_2104_06181()

    def draconic_pointer_rotations_per_b1_rotation(self) -> float:
        """Return s1 rotations per one b1 rotation (assuming r1 shares the a-shaft)."""
        angles_b1 = self._train.propagate(1.0, "b1")
        a1_angle = angles_b1["a1"]
        angles_r1 = self._train.propagate(a1_angle, "r1")
        return angles_r1["s1"]
