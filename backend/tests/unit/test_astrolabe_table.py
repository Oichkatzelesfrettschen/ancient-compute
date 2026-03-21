"""Tests for astrolabe emulator."""

import math

import pytest

from backend.src.emulator.astrolabe import AstrolabeEmulator, AstrolabeQuery


def test_astrolabe_solar_altitude_equinox_equator_noon():
    emu = AstrolabeEmulator()
    q = AstrolabeQuery(latitude_deg=0.0, date="2026-03-20", time="12:00", target="sun")
    assert emu.read_altitude(q) > 85.0


class TestAstrolabeEmulator:
    def _noon(self, lat: float, date: str) -> float:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=lat, date=date, time="12:00", target="sun")
        return emu.read_altitude(q)

    def test_noon_positive_equinox_northern(self) -> None:
        # 51.5N on vernal equinox: noon sun is above horizon
        assert self._noon(51.5, "2026-03-20") > 0.0

    def test_midnight_below_horizon(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="00:00", target="sun")
        assert emu.read_altitude(q) < 0.0

    def test_northern_winter_noon_lower_than_summer(self) -> None:
        winter = self._noon(51.5, "2026-12-21")
        summer = self._noon(51.5, "2026-06-21")
        assert winter < summer

    def test_equatorial_noon_equinox_near_90(self) -> None:
        alt = self._noon(0.0, "2026-03-20")
        assert abs(alt - 90.0) < 5.0

    def test_high_latitude_winter_noon_low(self) -> None:
        # 60N in December: sun barely above horizon
        alt = self._noon(60.0, "2026-12-21")
        assert alt < 20.0

    def test_equinox_equator_sunrise_near_zero(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-03-20", time="06:00", target="sun")
        alt = emu.read_altitude(q)
        assert abs(alt) < 10.0

    def test_altitude_always_in_valid_range(self) -> None:
        emu = AstrolabeEmulator()
        for lat in [-60, -30, 0, 30, 60]:
            for date in ["2026-03-20", "2026-06-21", "2026-09-23", "2026-12-21"]:
                for time in ["06:00", "12:00", "18:00"]:
                    q = AstrolabeQuery(
                        latitude_deg=float(lat), date=date, time=time, target="sun"
                    )
                    alt = emu.read_altitude(q)
                    assert -90.0 <= alt <= 90.0

    def test_southern_hemisphere_summer_noon_high(self) -> None:
        # 30S in December (southern summer): noon sun is high
        alt = self._noon(-30.0, "2026-12-21")
        assert alt > 60.0

    def test_non_solar_target_no_table_raises(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="22:00", target="jupiter")
        with pytest.raises(KeyError):
            emu.read_altitude(q)

    def test_solar_keyword_case_insensitive(self) -> None:
        emu = AstrolabeEmulator()
        q_sun = AstrolabeQuery(
            latitude_deg=51.5, date="2026-03-20", time="12:00", target="SUN"
        )
        q_solar = AstrolabeQuery(
            latitude_deg=51.5, date="2026-03-20", time="12:00", target="Solar"
        )
        alt_sun = emu.read_altitude(q_sun)
        alt_solar = emu.read_altitude(q_solar)
        assert math.isclose(alt_sun, alt_solar)

    def test_deterministic_same_query_same_result(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=48.8, date="2026-07-14", time="14:30", target="sun")
        assert emu.read_altitude(q) == emu.read_altitude(q)
