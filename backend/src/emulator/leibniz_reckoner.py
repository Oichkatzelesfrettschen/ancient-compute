"""Logic-only stub emulator for Leibniz stepped reckoner."""

from __future__ import annotations


class LeibnizReckonerEmulator:
    def __init__(self) -> None:
        self._accumulator = 0
        self._carriage = 0

    def reset(self) -> None:
        self._accumulator = 0
        self._carriage = 0

    def state(self) -> dict:
        return {
            "accumulator": self._accumulator,
            "carriage": self._carriage,
        }

    def add_cycle(self, value: int) -> int:
        self._accumulator += value
        return self._accumulator

    def shift_carriage(self, offset: int) -> int:
        self._carriage += offset
        return self._carriage
