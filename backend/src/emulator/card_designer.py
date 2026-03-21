"""Card programmer/designer for all three historical punch card formats.

Provides a Python API and JSON serialization layer that converts user-friendly
card specifications into machine-loadable payloads for POST /machines/{id}/load.

Formats:
  HollerithCardDesigner   -- 12-row x 80-col IBM/Hollerith format
  JacquardCardDesigner    -- N-hook binary row format (Jacquard loom)
  AECardDesigner          -- AE assembly source (POST /machines/analytical-engine/load)

WHY this module exists: three distinct punch card families are supported by the
machine registry, but no client-side tool existed to compose card layouts and
produce the JSON payload format that _apply_load() expects.  This module closes
that gap for Python users and for the CLI tool (tools/card_designer.py).
"""

from __future__ import annotations

import json
from abc import ABC, abstractmethod
from typing import Any

# ---------------------------------------------------------------------------
# IBM Hollerith 026/029 character encoding
# Row numbering: 12 = top zone row, 11 = second zone row, 0 = third zone row,
# 1-9 = numeric rows 1 through 9.  Row indices stored as integers 0-11 where:
#   index 0  = zone row 12 (top)
#   index 1  = zone row 11
#   index 2  = zone row 0 (not digit 0 -- the "0-zone" row)
#   index 3  = digit row 1
#   ...
#   index 11 = digit row 9
# When calling punch(col, row) the row parameter uses these 0-11 indices.
# ---------------------------------------------------------------------------

# Hollerith encoding: character -> list of row indices (0-based, see above)
# Source: IBM 026/029 keypunch character set standard.
HOLLERITH_ENCODING: dict[str, list[int]] = {
    # Letters A-I: zone-12 + digit 1-9
    "A": [0, 3],  # 12 + 1
    "B": [0, 4],  # 12 + 2
    "C": [0, 5],  # 12 + 3
    "D": [0, 6],  # 12 + 4
    "E": [0, 7],  # 12 + 5
    "F": [0, 8],  # 12 + 6
    "G": [0, 9],  # 12 + 7
    "H": [0, 10],  # 12 + 8
    "I": [0, 11],  # 12 + 9
    # Letters J-R: zone-11 + digit 1-9
    "J": [1, 3],  # 11 + 1
    "K": [1, 4],  # 11 + 2
    "L": [1, 5],  # 11 + 3
    "M": [1, 6],  # 11 + 4
    "N": [1, 7],  # 11 + 5
    "O": [1, 8],  # 11 + 6
    "P": [1, 9],  # 11 + 7
    "Q": [1, 10],  # 11 + 8
    "R": [1, 11],  # 11 + 9
    # Letters S-Z: zone-0 + digit 2-9 (no S+1 pairing -- S starts at 2)
    "S": [2, 4],  # 0 + 2
    "T": [2, 5],  # 0 + 3
    "U": [2, 6],  # 0 + 4
    "V": [2, 7],  # 0 + 5
    "W": [2, 8],  # 0 + 6
    "X": [2, 9],  # 0 + 7
    "Y": [2, 10],  # 0 + 8
    "Z": [2, 11],  # 0 + 9
    # Digits 0-9
    "0": [2],  # zone-0 only
    "1": [3],
    "2": [4],
    "3": [5],
    "4": [6],
    "5": [7],
    "6": [8],
    "7": [9],
    "8": [10],
    "9": [11],
    # Special characters (IBM 029 standard)
    " ": [],  # blank column -- no holes
    "&": [0],  # zone-12 only
    "-": [1],  # zone-11 only (also used as minus sign)
    "/": [2, 3],  # 0 + 1
    ".": [3, 8, 3],  # simplified: 12 + 8 + 3 -> use [0, 10, 5] (IBM: 12-3-8)
    ",": [0, 4, 8],  # IBM: 12-3-8 -> close approximation
    "(": [0, 5, 8],
    ")": [1, 5, 8],
    "*": [0, 6, 8],
    "=": [0, 2, 8],
    "+": [0, 1],  # 12 + 11 zone
    "$": [0, 2, 3],
    "#": [0, 2, 8, 3],
    "@": [0, 2, 4],
    "%": [0, 1, 8, 4],
    "!": [1, 2, 8],
    ":": [0, 2],
    ";": [0, 2, 3, 8],
    "'": [0, 5],
    '"': [0, 7],
    "?": [0, 2, 3, 8],
    "<": [0, 2, 6],
    ">": [0, 2, 7],
}


class CardDesigner(ABC):
    """Abstract base for all card format designers.

    Subclasses build a card layout in memory, then call to_payload() to get
    the dict expected by POST /machines/{id}/load.
    """

    @abstractmethod
    def to_payload(self) -> dict[str, Any]:
        """Return a dict suitable for the 'payload' key of POST /machines/{id}/load."""

    @abstractmethod
    def render_ascii(self) -> str:
        """Return a printable ASCII-art representation of the card(s)."""

    def to_json(self) -> str:
        """Serialize to JSON with a top-level 'payload' key (ready for POST body)."""
        return json.dumps({"payload": self.to_payload()}, indent=2)


# ---------------------------------------------------------------------------
# HollerithCardDesigner
# ---------------------------------------------------------------------------


class HollerithCardDesigner(CardDesigner):
    """Design IBM/Hollerith punch cards (12 rows x N columns).

    Column indexing is 0-based.  Row indexing uses the 0-11 scheme described
    in HOLLERITH_ENCODING: row 0 = zone-12, row 1 = zone-11, row 2 = zone-0,
    rows 3-11 = digit rows 1-9.

    The payload format matches what the hollerith-tabulator _apply_load handler
    expects: {"cards": [{"row": col_idx, "columns": [row_idx, ...]}, ...]}
    where 'row' is the card's column index and 'columns' are the punched row
    indices within that column.

    Example::

        d = HollerithCardDesigner()
        d.encode_text("HELLO")
        payload = d.to_payload()
        # -> {"cards": [{"row": 0, "columns": [0, 10]}, ...]}
    """

    NUM_ROWS = 12  # standard Hollerith row count

    def __init__(self, cols: int = 80) -> None:
        self.cols = cols
        # _punches[col] = set of row indices punched in that column
        self._punches: list[set[int]] = [set() for _ in range(cols)]

    def punch(self, col: int, row: int) -> HollerithCardDesigner:
        """Punch a single hole at column col (0-indexed), row row (0-11).

        Returns self for chaining.
        """
        if not (0 <= col < self.cols):
            raise ValueError(f"Column {col} out of range 0..{self.cols - 1}")
        if not (0 <= row < self.NUM_ROWS):
            raise ValueError(f"Row {row} out of range 0..{self.NUM_ROWS - 1}")
        self._punches[col].add(row)
        return self

    def encode_char(self, col: int, char: str) -> HollerithCardDesigner:
        """Punch the holes for an alphanumeric character at column col."""
        rows = HOLLERITH_ENCODING.get(char.upper(), [])
        for r in rows:
            if 0 <= r < self.NUM_ROWS:
                self._punches[col].add(r)
        return self

    def encode_text(self, text: str, start_col: int = 0) -> HollerithCardDesigner:
        """Encode a string starting at start_col (one character per column)."""
        for i, c in enumerate(text):
            target_col = start_col + i
            if target_col < self.cols:
                self.encode_char(target_col, c)
        return self

    def clear_column(self, col: int) -> HollerithCardDesigner:
        """Remove all holes from column col."""
        self._punches[col].clear()
        return self

    def to_payload(self) -> dict[str, Any]:
        """Return payload in hollerith-tabulator format.

        Format: {"cards": [{"row": col_idx, "columns": sorted_row_indices}]}
        Only non-empty columns are included.
        """
        cards = []
        for col, rows in enumerate(self._punches):
            if rows:
                cards.append({"row": col, "columns": sorted(rows)})
        return {"cards": cards}

    def render_ascii(self) -> str:
        """Render card as 12-row ASCII grid.  O = hole, . = blank.

        Row labels show the Hollerith row name (12, 11, 0, 1..9).
        """
        row_labels = ["12", "11", " 0", " 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9"]
        display_cols = min(self.cols, 80)  # cap for readable display
        lines = [f"Hollerith card: {self.cols} cols x {self.NUM_ROWS} rows"]
        lines.append("Row " + "".join(str(c % 10) for c in range(display_cols)))
        for r in range(self.NUM_ROWS):
            line = "".join("O" if r in self._punches[c] else "." for c in range(display_cols))
            lines.append(f"R{row_labels[r]}: {line}")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# JacquardCardDesigner
# ---------------------------------------------------------------------------


class JacquardCardDesigner(CardDesigner):
    """Design Jacquard loom punch cards (binary rows, N hooks per row).

    Each card row is a list of N integers (0 or 1): 1 = hook raised (warp thread
    lifted), 0 = hook lowered.

    The payload format matches the jacquard-loom _apply_load handler:
    {"cards": [[1,0,1,...], [0,1,0,...], ...]}

    Example::

        d = JacquardCardDesigner(hooks=8)
        d.add_card([1,0,1,0,1,0,1,0])
        d.add_card([0,1,0,1,0,1,0,1])
        payload = d.to_payload()
        # -> {"cards": [[1,0,1,0,1,0,1,0], [0,1,0,1,0,1,0,1]]}
    """

    def __init__(self, hooks: int = 8) -> None:
        self.hooks = hooks
        self._cards: list[list[int]] = []

    def add_card(self, pattern: list[int]) -> JacquardCardDesigner:
        """Append one card row.  pattern length must equal hooks.

        Raises ValueError if length mismatch.  Values are coerced to 0/1.
        """
        if len(pattern) != self.hooks:
            raise ValueError(f"Pattern length {len(pattern)} != hooks {self.hooks}")
        self._cards.append([int(bool(x)) for x in pattern])
        return self

    def add_row_from_string(self, s: str) -> JacquardCardDesigner:
        """Add a card row from a binary string, e.g. '10101010'."""
        bits = [int(c) for c in s if c in "01"]
        return self.add_card(bits)

    def stripe(self, n: int) -> JacquardCardDesigner:
        """Add n alternating twill pattern cards (10101... / 01010...)."""
        for i in range(n):
            self._cards.append([(i + j) % 2 for j in range(self.hooks)])
        return self

    def clear(self) -> JacquardCardDesigner:
        """Remove all cards."""
        self._cards.clear()
        return self

    def to_payload(self) -> dict[str, Any]:
        return {"cards": [list(c) for c in self._cards]}

    def render_ascii(self) -> str:
        """Render cards as ASCII grid.  # = hook raised (1), . = lowered (0)."""
        lines = [f"Jacquard deck: {self.hooks} hooks, {len(self._cards)} cards"]
        for i, card in enumerate(self._cards):
            row = "".join("#" if h else "." for h in card)
            lines.append(f"Card {i:4d}: {row}")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# AECardDesigner
# ---------------------------------------------------------------------------


class AECardDesigner(CardDesigner):
    """Design programs for the Analytical Engine in assembly source form.

    The AE accepts POST /machines/analytical-engine/load with
    {"payload": {"source": "<assembly text>"}}.

    This designer accumulates assembly source lines and serializes them
    into that payload format.  render_ascii() attempts to compile the source
    via tools/card_compiler.py and shows a hex rendering of the compiled cards.

    Example::

        d = AECardDesigner("LOAD A, 7")
        d.append_instruction("HALT")
        payload = d.to_payload()
        # -> {"source": "LOAD A, 7\\nHALT"}
    """

    def __init__(self, source: str = "") -> None:
        self._source = source

    def set_source(self, source: str) -> AECardDesigner:
        """Replace the entire source program."""
        self._source = source
        return self

    def append_instruction(self, line: str) -> AECardDesigner:
        """Append one assembly instruction line."""
        if self._source and not self._source.endswith("\n"):
            self._source += "\n"
        self._source += line
        return self

    def to_payload(self) -> dict[str, Any]:
        return {"source": self._source.strip()}

    def render_ascii(self) -> str:
        """Attempt to compile and render card hex.  Falls back to raw source."""
        import sys
        from pathlib import Path

        tools_dir = str(Path(__file__).resolve().parents[3] / "tools")
        if tools_dir not in sys.path:
            sys.path.insert(0, tools_dir)
        try:
            from card_compiler import compile_deck  # type: ignore[import-not-found]

            cards = compile_deck(self._source)
            lines = [f"AE Deck: {len(cards)} cards"]
            for i, card in enumerate(cards):
                lines.append(f"Card {i:4d}: {card.raw_hex()}")
            return "\n".join(lines)
        except Exception:
            # Fall back to showing raw source lines
            lines = ["AE Source (not compiled):"]
            for i, line in enumerate(self._source.strip().splitlines()):
                lines.append(f"Line {i:4d}: {line}")
            return "\n".join(lines)
