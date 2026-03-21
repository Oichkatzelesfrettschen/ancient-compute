"""Jacquard Loom emulator (France, 1804).

Joseph Marie Jacquard's power loom used a chain of stiff pasteboard punch
cards -- one per weft row -- to control which warp threads were raised or
lowered during each weaving pass. A hole in the card let a needle pass
through, raising the corresponding hook (and warp thread); no hole blocked
the needle, keeping the thread lowered.

This binary, punch-card-controlled mechanism directly inspired Charles
Babbage's use of punched cards for the Analytical Engine (1837), and is
widely regarded as the first example of a stored, reusable program.

Architecture emulated here:
  - JacquardLoom: a deck of punch cards (one bit-row per weft pass).
    step() advances through the deck one card at a time.
  - 0 = hook lowered (warp thread down; weft passes over).
  - 1 = hook raised (warp thread up; weft passes under).
  - Pattern output accumulates row by row to form the woven design.

References:
  - Jacquard, J. M. (1804). Brevet d'invention. French Patent No. 245.
  - Essinger, J. (2004). Jacquard's Web. Oxford University Press.
    ISBN 978-0-19-280578-2. pp. 47-63.
  - Randell, B. (1982). From analytical engine to electronic digital computer.
    IEEE Annals of the History of Computing, 4(4), 327-341.
"""

from __future__ import annotations

from dataclasses import dataclass, field


@dataclass
class JacquardState:
    """State of the Jacquard loom."""

    num_hooks: int = 8
    card_deck: list[list[int]] = field(default_factory=list)
    card_index: int = 0
    weft_count: int = 0
    pattern_output: list[list[int]] = field(default_factory=list)


class JacquardLoom:
    """Jacquard Loom emulator.

    Models the punch-card-driven hook control system. Each card in the deck
    is a row of 0/1 values (one per hook). A 1 means the hook is raised
    (warp thread up; weft passes under). A 0 means hook lowered.

    Usage:
        loom = JacquardLoom(num_hooks=8)
        loom.load_deck([
            [1, 0, 1, 0, 1, 0, 1, 0],  # card 1: odd hooks raised
            [0, 1, 0, 1, 0, 1, 0, 1],  # card 2: even hooks raised
        ])
        row = loom.step()              # process card 1 -> [1, 0, 1, 0, ...]
        row = loom.step()              # process card 2 -> [0, 1, 0, 1, ...]
        pattern = loom.get_pattern()   # [[1,0,...], [0,1,...]]
    """

    def __init__(self, num_hooks: int = 8) -> None:
        self.state = JacquardState(num_hooks=num_hooks)

    @property
    def num_hooks(self) -> int:
        return self.state.num_hooks

    @property
    def weft_count(self) -> int:
        return self.state.weft_count

    @property
    def card_index(self) -> int:
        return self.state.card_index

    def load_deck(self, cards: list[list[int]]) -> None:
        """Load punch card deck; each card is a list of 0/1 per hook."""
        for i, card in enumerate(cards):
            if len(card) != self.state.num_hooks:
                raise ValueError(
                    f"Card {i}: expected {self.state.num_hooks} hooks, got {len(card)}"
                )
        self.state.card_deck = [list(c) for c in cards]
        self.state.card_index = 0
        self.state.weft_count = 0
        self.state.pattern_output = []

    def step(self) -> list[int] | None:
        """Process one card: advance pattern by one weft row.

        Returns the hook pattern for this row (1=raised, 0=lowered),
        or None if past end of deck (loom stopped).
        """
        if self.state.card_index >= len(self.state.card_deck):
            return None
        card = self.state.card_deck[self.state.card_index]
        self.state.pattern_output.append(list(card))
        self.state.card_index += 1
        self.state.weft_count += 1
        return list(card)

    def get_pattern(self) -> list[list[int]]:
        """Return accumulated woven pattern (one row per weft pass)."""
        return list(self.state.pattern_output)

    def reset(self) -> None:
        """Reset to start of deck without clearing it (re-weave same design)."""
        self.state.card_index = 0
        self.state.weft_count = 0
        self.state.pattern_output = []

    def read_card(self, card: list[int]) -> list[int]:
        """Read a single card without loading a deck (legacy / one-shot API)."""
        if len(card) != self.state.num_hooks:
            raise ValueError(
                f"Card length {len(card)} does not match hook count {self.state.num_hooks}"
            )
        return [1 if bit else 0 for bit in card]

    def state_dict(self) -> dict[str, object]:
        """Return machine state as a plain dict."""
        return {
            "num_hooks": self.state.num_hooks,
            "weft_count": self.state.weft_count,
            "card_index": self.state.card_index,
            "deck_size": len(self.state.card_deck),
            "pattern": self.get_pattern(),
        }


class JacquardEmulator(JacquardLoom):
    """Backward-compatible alias for JacquardLoom.

    WHY: original API used JacquardEmulator(hooks=N); new canonical class
    is JacquardLoom(num_hooks=N). This bridges the two.
    """

    def __init__(self, hooks: int = 8, num_hooks: int | None = None) -> None:
        super().__init__(num_hooks=num_hooks if num_hooks is not None else hooks)
