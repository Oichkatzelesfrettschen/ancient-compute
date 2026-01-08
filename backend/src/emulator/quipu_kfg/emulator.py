"""Tier-1 quipu emulator driven by KFG normalized artifacts."""

from __future__ import annotations

from dataclasses import dataclass

from .parser import KFGArtifact


@dataclass(frozen=True)
class QuipuSummary:
    investigator_num: str
    cord_count: int
    total_value: int


class QuipuKFGEmulator:
    def __init__(self, artifact: KFGArtifact) -> None:
        self.artifact = artifact

    def total_value(self) -> int:
        total = 0
        for cord in self.artifact.cords:
            try:
                total += int(cord.get("Value", 0) or 0)
            except ValueError:
                continue
        return total

    def summarize(self) -> QuipuSummary:
        return QuipuSummary(
            investigator_num=self.artifact.investigator_num,
            cord_count=len(self.artifact.cords),
            total_value=self.total_value(),
        )
