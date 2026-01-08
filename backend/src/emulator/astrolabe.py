"""Table-driven astrolabe emulator (placeholder).

This emulator answers queries by looking up entries in a reference table.
The reference table must be replaced with extracted primary tables.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class AstrolabeQuery:
    latitude_deg: float
    date: str
    time: str
    target: str


class AstrolabeEmulator:
    def __init__(self, table_path: Path) -> None:
        self._table = json.loads(table_path.read_text(encoding="utf-8"))

    def read_altitude(self, query: AstrolabeQuery) -> float:
        for e in self._table.get("entries", []):
            if (
                float(e.get("latitude_deg")) == float(query.latitude_deg)
                and e.get("date") == query.date
                and e.get("time") == query.time
                and e.get("target") == query.target
            ):
                return float(e.get("altitude_deg"))
        raise KeyError("No matching entry")
