"""Logic-only emulator for Jacquard loom card decoding."""

from __future__ import annotations


class JacquardEmulator:
    def __init__(self, hooks: int) -> None:
        self.hooks = hooks

    def read_card(self, card: list[int]) -> list[int]:
        if len(card) != self.hooks:
            raise ValueError("Card length must match hook count")
        return [1 if bit else 0 for bit in card]
