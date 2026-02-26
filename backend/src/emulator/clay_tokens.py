"""Logic-only emulator for clay tokens and bullae."""

from __future__ import annotations

from collections import Counter


class ClayTokensEmulator:
    def __init__(self) -> None:
        self._tokens: Counter[str] = Counter()
        self._sealed = False
        self._impression: Counter[str] = Counter()

    def reset(self) -> None:
        self._tokens = Counter()
        self._sealed = False
        self._impression = Counter()

    def state(self) -> dict:
        return {
            "tokens": dict(self._tokens),
            "sealed": self._sealed,
            "impression": dict(self._impression),
        }

    def add_token(self, token_type: str, qty: int = 1) -> None:
        self._require_unsealed()
        self._tokens[token_type] += qty

    def remove_token(self, token_type: str, qty: int = 1) -> None:
        self._require_unsealed()
        self._tokens[token_type] = max(0, self._tokens[token_type] - qty)
        if self._tokens[token_type] == 0:
            self._tokens.pop(token_type, None)

    def seal(self) -> None:
        self._sealed = True
        self._impression = Counter(self._tokens)

    def audit(self) -> bool:
        return self._tokens == self._impression

    def _require_unsealed(self) -> None:
        if self._sealed:
            raise ValueError("Cannot mutate sealed bulla")
