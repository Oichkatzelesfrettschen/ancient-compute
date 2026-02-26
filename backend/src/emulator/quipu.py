"""Logic-only emulator for quipu/khipu encoding."""

from __future__ import annotations


class QuipuEmulator:
    def __init__(self) -> None:
        self._records: list[dict[str, str | int]] = []

    def reset(self) -> None:
        self._records = []

    def state(self) -> dict:
        return {
            "records": list(self._records),
        }

    def encode_number(self, category: str, value: int) -> None:
        self._records.append({"category": category, "value": value})

    def decode_number(self, category: str) -> list[int]:
        return [int(r["value"]) for r in self._records if r["category"] == category]

    def sum_by_category(self, category: str) -> int:
        return sum(self.decode_number(category))
