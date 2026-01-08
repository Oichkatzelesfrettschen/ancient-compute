"""Tier-2-ish gear-train propagation model for Antikythera-like mechanisms.

This is a generic gear train model (ratio propagation). Real Antikythera gear
ratios and dial mappings must be sourced and loaded separately.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class GearEdge:
    src: str
    dst: str
    ratio: float


class GearTrain:
    def __init__(self, edges: list[GearEdge]) -> None:
        self._edges = edges

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
