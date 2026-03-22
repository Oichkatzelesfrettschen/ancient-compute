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
                    q = AstrolabeQuery(latitude_deg=float(lat), date=date, time=time, target="sun")
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
        q_sun = AstrolabeQuery(latitude_deg=51.5, date="2026-03-20", time="12:00", target="SUN")
        q_solar = AstrolabeQuery(latitude_deg=51.5, date="2026-03-20", time="12:00", target="Solar")
        alt_sun = emu.read_altitude(q_sun)
        alt_solar = emu.read_altitude(q_solar)
        assert math.isclose(alt_sun, alt_solar)

    def test_deterministic_same_query_same_result(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=48.8, date="2026-07-14", time="14:30", target="sun")
        assert emu.read_altitude(q) == emu.read_altitude(q)


class TestAstrolabeQuery:
    """AstrolabeQuery dataclass field tests."""

    def test_query_stores_latitude(self) -> None:
        q = AstrolabeQuery(latitude_deg=48.8, date="2026-01-01", time="12:00", target="sun")
        assert q.latitude_deg == 48.8

    def test_query_stores_date(self) -> None:
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-06-21", time="12:00", target="sun")
        assert q.date == "2026-06-21"

    def test_query_stores_time(self) -> None:
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-01-01", time="15:30", target="sun")
        assert q.time == "15:30"

    def test_query_stores_target(self) -> None:
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-01-01", time="12:00", target="sun")
        assert q.target == "sun"


class TestAstrolabeEmulatorAdditional:
    """Edge cases and additional properties of the astrolabe emulator."""

    def test_north_pole_arctic_summer(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=90.0, date="2026-06-21", time="12:00", target="sun")
        alt = emu.read_altitude(q)
        # Sun is above horizon (circumpolar in Arctic summer)
        assert alt > 0.0

    def test_altitude_is_float(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-03-20", time="12:00", target="sun")
        assert isinstance(emu.read_altitude(q), float)

    def test_different_latitudes_give_different_noon_altitudes(self) -> None:
        emu = AstrolabeEmulator()
        q_equator = AstrolabeQuery(latitude_deg=0.0, date="2026-06-21", time="12:00", target="sun")
        q_north = AstrolabeQuery(latitude_deg=60.0, date="2026-06-21", time="12:00", target="sun")
        alt_equator = emu.read_altitude(q_equator)
        alt_north = emu.read_altitude(q_north)
        assert alt_equator != alt_north

    def test_summer_noon_higher_than_equinox_noon_at_50n(self) -> None:
        emu = AstrolabeEmulator()
        q_summer = AstrolabeQuery(latitude_deg=50.0, date="2026-06-21", time="12:00", target="sun")
        q_equinox = AstrolabeQuery(latitude_deg=50.0, date="2026-03-20", time="12:00", target="sun")
        assert emu.read_altitude(q_summer) > emu.read_altitude(q_equinox)

    def test_morning_lower_than_noon(self) -> None:
        emu = AstrolabeEmulator()
        q_morning = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="06:00", target="sun")
        q_noon = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="12:00", target="sun")
        assert emu.read_altitude(q_morning) < emu.read_altitude(q_noon)


# ---------------------------------------------------------------------------
# Astronomical accuracy
# ---------------------------------------------------------------------------


class TestAstrolabeAstronomicalAccuracy:
    """Solar altitude model produces physically plausible values."""

    def test_noon_sun_angle_increases_from_high_to_low_latitude(self) -> None:
        # At equinox, noon sun is higher at lower latitudes
        emu = AstrolabeEmulator()
        latitudes = [60.0, 45.0, 30.0, 0.0]
        altitudes = [
            emu.read_altitude(AstrolabeQuery(lat, "2026-03-20", "12:00", "sun"))
            for lat in latitudes
        ]
        # Monotonically increasing (higher altitude at lower latitude)
        for i in range(len(altitudes) - 1):
            assert altitudes[i] < altitudes[i + 1]

    def test_south_pole_winter_altitude_negative(self) -> None:
        # 90S in June: deep polar night
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=-90.0, date="2026-06-21", time="12:00", target="sun")
        assert emu.read_altitude(q) < 0.0

    def test_sunset_altitude_near_zero_equinox_equator(self) -> None:
        # At 18:00 on equinox at equator, sun near horizon
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-03-20", time="18:00", target="sun")
        alt = emu.read_altitude(q)
        assert abs(alt) < 15.0

    def test_solstice_noon_altitude_difference_is_large(self) -> None:
        # Difference between summer and winter solstice noon altitude at 51N > 40 degrees
        emu = AstrolabeEmulator()
        summer = emu.read_altitude(
            AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="12:00", target="sun")
        )
        winter = emu.read_altitude(
            AstrolabeQuery(latitude_deg=51.5, date="2026-12-21", time="12:00", target="sun")
        )
        assert (summer - winter) > 40.0

    def test_midnight_sun_altitude_at_arctic_summer(self) -> None:
        # 70N in June at midnight: sun above horizon
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=70.0, date="2026-06-21", time="00:00", target="sun")
        alt = emu.read_altitude(q)
        assert alt > 0.0

    def test_extreme_time_23_59_produces_valid_altitude(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="23:59", target="sun")
        alt = emu.read_altitude(q)
        assert -90.0 <= alt <= 90.0

    def test_time_00_00_produces_valid_altitude(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-01-01", time="00:00", target="sun")
        alt = emu.read_altitude(q)
        assert -90.0 <= alt <= 90.0

    def test_southern_latitude_summer_noon_altitude(self) -> None:
        # 30S in December (southern summer): sun altitude > equinox altitude at 30S
        emu = AstrolabeEmulator()
        december = emu.read_altitude(
            AstrolabeQuery(latitude_deg=-30.0, date="2026-12-21", time="12:00", target="sun")
        )
        march = emu.read_altitude(
            AstrolabeQuery(latitude_deg=-30.0, date="2026-03-20", time="12:00", target="sun")
        )
        assert december > march

    def test_altitude_symmetric_morning_afternoon(self) -> None:
        # 09:00 and 15:00 should give same altitude (symmetric around noon solar time)
        emu = AstrolabeEmulator()
        q_am = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="09:00", target="sun")
        q_pm = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="15:00", target="sun")
        assert abs(emu.read_altitude(q_am) - emu.read_altitude(q_pm)) < 1.0

    def test_altitude_range_all_seasons_all_latitudes(self) -> None:
        emu = AstrolabeEmulator()
        for lat in range(-80, 81, 20):
            for month_day in ["01-15", "04-15", "07-15", "10-15"]:
                q = AstrolabeQuery(
                    latitude_deg=float(lat),
                    date=f"2026-{month_day}",
                    time="12:00",
                    target="sun",
                )
                alt = emu.read_altitude(q)
                assert -90.0 <= alt <= 90.0


# ---------------------------------------------------------------------------
# AstrolabeQuery dataclass properties
# ---------------------------------------------------------------------------


class TestAstrolabeQueryExtended:
    """AstrolabeQuery: frozen, equality, hashability."""

    def test_query_is_frozen(self) -> None:
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="12:00", target="sun")
        with pytest.raises((AttributeError, TypeError)):
            q.latitude_deg = 0.0  # type: ignore[misc]

    def test_two_identical_queries_are_equal(self) -> None:
        q1 = AstrolabeQuery(latitude_deg=48.8, date="2026-07-14", time="14:30", target="sun")
        q2 = AstrolabeQuery(latitude_deg=48.8, date="2026-07-14", time="14:30", target="sun")
        assert q1 == q2

    def test_two_different_queries_not_equal(self) -> None:
        q1 = AstrolabeQuery(latitude_deg=48.8, date="2026-07-14", time="14:30", target="sun")
        q2 = AstrolabeQuery(latitude_deg=48.9, date="2026-07-14", time="14:30", target="sun")
        assert q1 != q2

    def test_query_is_hashable(self) -> None:
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-01-01", time="12:00", target="sun")
        s = {q}
        assert len(s) == 1

    def test_negative_latitude_stored(self) -> None:
        q = AstrolabeQuery(latitude_deg=-33.9, date="2026-01-01", time="12:00", target="sun")
        assert q.latitude_deg == -33.9


class TestAstrolabeQueryEdgeCases:
    """AstrolabeQuery field validation and emulator edge cases."""

    def _alt(self, lat: float, date: str, time: str = "12:00") -> float:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=lat, date=date, time=time, target="sun")
        return emu.read_altitude(q)

    def test_high_latitude_noon_equinox_positive(self) -> None:
        # Oslo (59.9N) noon vernal equinox: sun above horizon
        assert self._alt(59.9, "2026-03-20") > 0.0

    def test_equator_noon_summer_solstice_high(self) -> None:
        alt = self._alt(0.0, "2026-06-21")
        assert alt > 60.0

    def test_south_pole_noon_equinox_below_horizon(self) -> None:
        alt = self._alt(-90.0, "2026-03-20")
        assert alt < 5.0  # near horizon or below

    def test_altitude_is_float(self) -> None:
        alt = self._alt(0.0, "2026-03-20")
        assert isinstance(alt, float)

    def test_altitude_never_exceeds_ninety(self) -> None:
        alt = self._alt(0.0, "2026-06-21")
        assert alt <= 90.0

    def test_query_target_field(self) -> None:
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-03-20", time="12:00", target="sun")
        assert q.target == "sun"

    def test_query_latitude_stored(self) -> None:
        q = AstrolabeQuery(latitude_deg=23.5, date="2026-12-21", time="12:00", target="sun")
        assert q.latitude_deg == pytest.approx(23.5)


class TestAstrolabeSeasonalVariation:
    """Seasonal altitude variation at fixed latitude."""

    def _alt(self, date: str, lat: float = 51.5) -> float:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=lat, date=date, time="12:00", target="sun")
        return emu.read_altitude(q)

    def test_summer_solstice_higher_than_equinox(self) -> None:
        equinox = self._alt("2026-03-20")
        summer = self._alt("2026-06-21")
        assert summer > equinox

    def test_winter_solstice_lower_than_equinox(self) -> None:
        equinox = self._alt("2026-03-20")
        winter = self._alt("2026-12-21")
        assert winter < equinox

    def test_equinoxes_approximately_equal(self) -> None:
        vernal = self._alt("2026-03-20")
        autumnal = self._alt("2026-09-22")
        assert abs(vernal - autumnal) < 3.0  # within 3 degrees

    def test_noon_altitude_positive_at_mid_latitude_summer(self) -> None:
        assert self._alt("2026-07-01") > 30.0

    def test_altitude_numerical_stability(self) -> None:
        # Multiple calls with same args give same result
        a1 = self._alt("2026-03-20")
        a2 = self._alt("2026-03-20")
        assert math.isclose(a1, a2)


class TestAstrolabeQueryDefaults:
    """AstrolabeQuery field types and defaults."""

    def test_latitude_stored(self) -> None:
        q = AstrolabeQuery(latitude_deg=45.0, date="2026-06-01", time="12:00", target="sun")
        assert q.latitude_deg == 45.0

    def test_target_stored(self) -> None:
        q = AstrolabeQuery(latitude_deg=0.0, date="2026-01-01", time="12:00", target="moon")
        assert q.target == "moon"

    def test_altitude_result_is_float(self) -> None:
        emu = AstrolabeEmulator()
        q = AstrolabeQuery(latitude_deg=51.5, date="2026-06-21", time="12:00", target="sun")
        assert isinstance(emu.read_altitude(q), float)
