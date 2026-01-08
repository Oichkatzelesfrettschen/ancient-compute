"""Astrolabe emulator.

Two modes are supported:
1) Computed: a compact spherical-astronomy model for solar altitude.
2) Table-driven: optional lookup for historically specific tables (WIP).
"""

from __future__ import annotations

import datetime as dt
import json
import math
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class AstrolabeQuery:
    latitude_deg: float
    date: str
    time: str
    target: str


class AstrolabeEmulator:
    def __init__(self, table_path: Path | None = None) -> None:
        self._table = None
        if table_path is not None:
            self._table = json.loads(table_path.read_text(encoding="utf-8"))

    def read_altitude(self, query: AstrolabeQuery) -> float:
        if query.target.lower() in {"sun", "solar"}:
            return self._solar_altitude(query.latitude_deg, query.date, query.time)

        if self._table is None:
            raise KeyError("No table loaded for non-solar target")

        for e in self._table.get("entries", []):
            if (
                float(e.get("latitude_deg")) == float(query.latitude_deg)
                and e.get("date") == query.date
                and e.get("time") == query.time
                and e.get("target") == query.target
            ):
                return float(e.get("altitude_deg"))
        raise KeyError("No matching entry")

    @staticmethod
    def _solar_altitude(latitude_deg: float, date: str, time: str) -> float:
        """Approximate solar altitude (degrees) for given local date/time.

        This is a compact, deterministic approximation intended for emulator-level
        logic, not high-precision ephemerides. We treat the provided time as local
        solar time (no timezones/equation-of-time correction yet).
        """
        day = dt.date.fromisoformat(date)
        hh, mm = (int(p) for p in time.split(":", 1))
        minutes = hh * 60 + mm

        # Day-of-year angle (radians) for simple declination approximation.
        n = day.timetuple().tm_yday
        gamma = 2.0 * math.pi * (n - 1) / 365.0

        # Solar declination (radians): common low-order approximation.
        decl = (
            0.006918
            - 0.399912 * math.cos(gamma)
            + 0.070257 * math.sin(gamma)
            - 0.006758 * math.cos(2 * gamma)
            + 0.000907 * math.sin(2 * gamma)
            - 0.002697 * math.cos(3 * gamma)
            + 0.00148 * math.sin(3 * gamma)
        )

        # Hour angle: treat 12:00 as local solar noon.
        hour_angle = math.radians((minutes / 4.0) - 180.0)

        lat = math.radians(latitude_deg)
        sin_alt = math.sin(lat) * math.sin(decl) + math.cos(lat) * math.cos(decl) * math.cos(hour_angle)
        sin_alt = max(-1.0, min(1.0, sin_alt))
        return math.degrees(math.asin(sin_alt))
