"""Logic-only emulator for counting rods/abacus arithmetic."""

from __future__ import annotations


class AbacusEmulator:
    def __init__(self) -> None:
        self._value = 0

    def reset(self) -> None:
        self._value = 0

    def state(self) -> dict:
        return {
            "value": self._value,
            "digits": self._digits(),
        }

    def set_value(self, value: int) -> None:
        self._value = max(0, value)

    def add(self, operand: int) -> int:
        self._value += operand
        return self._value

    def sub(self, operand: int) -> int:
        self._value = max(0, self._value - operand)
        return self._value

    def _digits(self) -> list[int]:
        if self._value == 0:
            return [0]
        digits = []
        value = self._value
        while value:
            digits.append(value % 10)
            value //= 10
        return list(reversed(digits))
