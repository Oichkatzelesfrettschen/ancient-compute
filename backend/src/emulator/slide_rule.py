"""Logic-only emulator for slide rule multiplication/division."""

from __future__ import annotations

import math


class SlideRuleEmulator:
    def multiply(self, a: float, b: float) -> float:
        self._require_positive(a, b)
        return math.exp(math.log(a) + math.log(b))

    def divide(self, a: float, b: float) -> float:
        self._require_positive(a, b)
        return math.exp(math.log(a) - math.log(b))

    def _require_positive(self, *values: float) -> None:
        if any(v <= 0 for v in values):
            raise ValueError("Slide rule inputs must be positive")
