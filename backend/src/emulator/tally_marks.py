"""Logic-only emulator for tally marks."""

from __future__ import annotations


class TallyMarksEmulator:
    def __init__(self) -> None:
        self._count = 0

    def step(self, delta: int) -> int:
        if delta < 0:
            self._count = max(0, self._count + delta)
        else:
            self._count += delta
        return self._count

    def run(self, deltas: list[int]) -> int:
        for delta in deltas:
            self.step(delta)
        return self._count

    def state(self) -> dict:
        return {
            "count": self._count,
            "tally": self.render(),
        }

    def reset(self) -> None:
        self._count = 0

    def render(self) -> str:
        groups = self._count // 5
        remainder = self._count % 5
        return ("|||||" * groups) + ("|" * remainder)
